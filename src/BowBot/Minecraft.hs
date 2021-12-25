{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module BowBot.Minecraft where

import Discord.Types
import BowBot.BotData
import Network.HTTP.Conduit (Manager)
import BowBot.API.Mojang
import BowBot.Utils
import Data.Char (toLower, isDigit)
import Data.List (sortOn, isPrefixOf, isSuffixOf)
import BowBot.API (APIMonad)


data MinecraftResponse e a
  = JustResponse String a
  | OldResponse String String a
  | PlayerNotFound
  | UserError e
  | DiscordUserNotFound
  | NotOnList
  | DidYouMeanResponse String a
  | DidYouMeanOldResponse String String a

mcNameToUUID :: APIMonad m => BotData -> String -> m (Maybe String)
mcNameToUUID bdt name = do
  mcAcc <- readProp minecraftAccounts bdt
  let goodAcc = filter ((==name) . head . mcNames) mcAcc
  case goodAcc of
    [MinecraftAccount {mcUUID}] -> return (Just mcUUID)
    _ -> mojangNameToUUID name

mcUUIDToNames :: APIMonad m => BotData -> String -> m (Maybe [String])
mcUUIDToNames bdt uuid = do
  mcAcc <- readProp minecraftAccounts bdt
  let goodAcc = filter ((==uuid) . mcUUID) mcAcc
  case goodAcc of
    [MinecraftAccount {mcNames}] -> return (Just mcNames)
    _ -> mojangUUIDToNames uuid

fromPingDiscordUser :: String -> Maybe UserId
fromPingDiscordUser str | "<@" `isPrefixOf` str && ">" `isSuffixOf` str = readMaybe $ filter isDigit str
fromPingDiscordUser _ = Nothing

withMinecraft :: APIMonad m => BotData -> Bool -> Either String UserId -> (String -> [String] -> m (Either e a)) -> m (MinecraftResponse e a)
withMinecraft bdt _ (Right author) fun = withMinecraftDiscord bdt author fun
withMinecraft bdt _ (Left (fromPingDiscordUser -> Just did)) fun = do
  ret <- withMinecraftDiscord bdt did fun
  return $ case ret of
    NotOnList -> DiscordUserNotFound
    _ -> ret
withMinecraft bdt ac (Left mcname) fun = 
  if ac then withMinecraftAutocorrect bdt True mcname fun else withMinecraftNormal bdt True mcname fun

withMinecraftDiscord :: APIMonad m => BotData -> UserId -> (String -> [String] -> m (Either e a)) -> m (MinecraftResponse e a)
withMinecraftDiscord bdt did fun = do
  pns <- fmap (>>=(\BowBotAccount {..} -> (,accountSelectedMinecraft) <$> accountDiscords)) $ readProp bowBotAccounts bdt
  case lookup did pns of
    Nothing -> pure NotOnList
    Just uuid -> do
      names <- fromMaybe [] <$> mcUUIDToNames bdt uuid
      res <- fun uuid names
      case res of
        Left e -> pure (UserError e)
        Right r -> pure (JustResponse (head names) r)

withMinecraftNormal :: APIMonad m => BotData -> Bool -> String -> (String -> [String] -> m (Either e a)) -> m (MinecraftResponse e a)
withMinecraftNormal bdt cont mcname fun = do
  maybeUUID <- mcNameToUUID bdt mcname
  case maybeUUID of
    Nothing -> if cont then withMinecraftAutocorrect bdt False mcname fun else pure PlayerNotFound
    Just uuid -> do
      names <- fromMaybe [] <$> mcUUIDToNames bdt uuid
      res <- fun uuid names
      case res of
        Left e -> if cont then withMinecraftAutocorrect bdt False mcname fun else pure (UserError e)
        Right r -> pure (JustResponse (head names) r)

withMinecraftAutocorrect :: APIMonad m => BotData -> Bool -> String -> (String -> [String] -> m (Either e a)) -> m (MinecraftResponse e a)
withMinecraftAutocorrect bdt cont mcname fun = do
  people <- readProp minecraftAccounts bdt
  let process f = let 
        nicks = [(mcUUID,u) | MinecraftAccount {..} <- people, u <- f mcNames]
        dists = map (\(u,n) -> ((u, n), dist (map toLower n) (map toLower mcname))) nicks
          in map fst . sortOn snd . filter (\(_,d) -> d <= 2) $ dists
  let filtered = process (take 1) ++ process (drop 1)
  case filtered of
    [] -> if cont then withMinecraftNormal bdt False mcname fun else pure PlayerNotFound
    ((uuid, rn):_) -> do
      names <- fromMaybe [] <$> mcUUIDToNames bdt uuid
      res <- fun uuid names
      case res of
        Left e -> pure (UserError e)
        Right r -> 
          if dist (map toLower $ head names) (map toLower mcname) <= 2
          then if map toLower mcname == map toLower rn
            then pure (JustResponse rn r)
            else pure (DidYouMeanResponse rn r)
          else if map toLower mcname == map toLower rn
            then pure (OldResponse rn (head names) r)
            else pure (DidYouMeanOldResponse rn (head names) r)