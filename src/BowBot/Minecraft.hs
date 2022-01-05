{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module BowBot.Minecraft where

import Discord.Types
import BowBot.BotData
import BowBot.API.Mojang
import BowBot.Utils
import Data.Char (toLower, isDigit)
import Data.List (sortOn, isPrefixOf, isSuffixOf)
import BowBot.CommandMonads


data MinecraftResponse e a
  = JustResponse String a
  | OldResponse String String a
  | PlayerNotFound
  | UserError e
  | DiscordUserNotFound
  | NotOnList
  | DidYouMeanResponse String a
  | DidYouMeanOldResponse String String a

mcNameToUUID :: (BotDataMonad m, APIMonad m) => String -> m (Maybe UUID)
mcNameToUUID name = do
  mcAcc <- hRead minecraftAccounts
  let goodAcc = filter ((==name) . head . mcNames) mcAcc
  case goodAcc of
    [MinecraftAccount {mcUUID}] -> return (Just mcUUID)
    _ -> mojangNameToUUID name

mcUUIDToNames :: (BotDataMonad m, APIMonad m) => UUID -> m (Maybe [String])
mcUUIDToNames uuid = do
  mcAcc <- hRead minecraftAccounts
  let goodAcc = filter ((==uuid) . mcUUID) mcAcc
  case goodAcc of
    [MinecraftAccount {mcNames}] -> return (Just mcNames)
    _ -> mojangUUIDToNames uuid

fromPingDiscordUser :: String -> Maybe UserId
fromPingDiscordUser str | "<@" `isPrefixOf` str && ">" `isSuffixOf` str = readMaybe $ filter isDigit str
fromPingDiscordUser _ = Nothing

withMinecraft :: (BotDataMonad m, APIMonad m) => Bool -> Either String UserId -> (UUID -> [String] -> m (Either e a)) -> m (MinecraftResponse e a)
withMinecraft _ (Right author) fun = withMinecraftDiscord author fun
withMinecraft _ (Left (fromPingDiscordUser -> Just did)) fun = do
  ret <- withMinecraftDiscord did fun
  return $ case ret of
    NotOnList -> DiscordUserNotFound
    _ -> ret
withMinecraft ac (Left mcname) fun = 
  if ac then withMinecraftAutocorrect True mcname fun else withMinecraftNormal True mcname fun

withMinecraftDiscord :: (BotDataMonad m, APIMonad m) => UserId -> (UUID -> [String] -> m (Either e a)) -> m (MinecraftResponse e a)
withMinecraftDiscord did fun = do
  pns <- fmap (>>=(\BowBotAccount {..} -> (,accountSelectedMinecraft) <$> accountDiscords)) $ hRead bowBotAccounts
  case lookup did pns of
    Nothing -> pure NotOnList
    Just uuid -> do
      names <- fromMaybe [] <$> mcUUIDToNames uuid
      res <- fun uuid names
      case res of
        Left e -> pure (UserError e)
        Right r -> pure (JustResponse (head names) r)

withMinecraftNormal :: (BotDataMonad m, APIMonad m) => Bool -> String -> (UUID -> [String] -> m (Either e a)) -> m (MinecraftResponse e a)
withMinecraftNormal cont mcname fun = do
  maybeUUID <- mcNameToUUID mcname
  case maybeUUID of
    Nothing -> if cont then withMinecraftAutocorrect False mcname fun else pure PlayerNotFound
    Just uuid -> do
      names <- fromMaybe [] <$> mcUUIDToNames uuid
      res <- fun uuid names
      case res of
        Left e -> if cont then withMinecraftAutocorrect False mcname fun else pure (UserError e)
        Right r -> pure (JustResponse (head names) r)

withMinecraftAutocorrect :: (BotDataMonad m, APIMonad m) => Bool -> String -> (UUID -> [String] -> m (Either e a)) -> m (MinecraftResponse e a)
withMinecraftAutocorrect cont mcname fun = do
  people <- hRead minecraftAccounts
  let process f = let 
        nicks = [(mcUUID,u) | MinecraftAccount {..} <- people, u <- f mcNames]
        dists = map (\(u,n) -> ((u, n), dist (map toLower n) (map toLower mcname))) nicks
          in map fst . sortOn snd . filter (\(_,d) -> d <= 2) $ dists
  let filtered = process (take 1) ++ process (drop 1)
  case filtered of
    [] -> if cont then withMinecraftNormal False mcname fun else pure PlayerNotFound
    ((uuid, rn):_) -> do
      names <- fromMaybe [] <$> mcUUIDToNames uuid
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