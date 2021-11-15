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


data MinecraftResponse e a
  = JustResponse String a
  | OldResponse String String a
  | PlayerNotFound
  | UserError e
  | DiscordUserNotFound
  | NotOnList
  | DidYouMeanResponse String a
  | DidYouMeanOldResponse String String a

mcNameToUUID :: Manager -> BotData -> String -> IO (Maybe String)
mcNameToUUID manager bdt name = do
  mcAcc <- readProp minecraftAccounts bdt
  let goodAcc = filter ((==name) . head . mcNames) mcAcc
  case goodAcc of
    [MinecraftAccount {mcUUID}] -> return (Just mcUUID)
    _ -> mojangNameToUUID manager name

mcUUIDToNames :: Manager -> BotData -> String -> IO (Maybe [String])
mcUUIDToNames manager bdt uuid = do
  mcAcc <- readProp minecraftAccounts bdt
  let goodAcc = filter ((==uuid) . mcUUID) mcAcc
  case goodAcc of
    [MinecraftAccount {mcNames}] -> return (Just mcNames)
    _ -> mojangUUIDToNames manager uuid

fromPingDiscordUser :: String -> Maybe UserId
fromPingDiscordUser str | "<@" `isPrefixOf` str && ">" `isSuffixOf` str = readMaybe $ filter isDigit str
fromPingDiscordUser _ = Nothing

withMinecraft :: MonadIO m => Manager -> BotData -> Bool -> Either String UserId -> (String -> [String] -> m (Either e a)) -> m (MinecraftResponse e a)
withMinecraft manager bdt _ (Right author) fun = withMinecraftDiscord manager bdt author fun
withMinecraft manager bdt _ (Left (fromPingDiscordUser -> Just did)) fun = do
  ret <- withMinecraftDiscord manager bdt did fun
  return $ case ret of
    NotOnList -> DiscordUserNotFound
    _ -> ret
withMinecraft manager bdt ac (Left mcname) fun = 
  if ac then withMinecraftAutocorrect manager bdt True mcname fun else withMinecraftNormal manager bdt True mcname fun

withMinecraftDiscord :: MonadIO m => Manager -> BotData -> UserId -> (String -> [String] -> m (Either e a)) -> m (MinecraftResponse e a)
withMinecraftDiscord manager bdt did fun = do
  pns <- fmap (>>=(\BowBotAccount {..} -> (,accountSelectedMinecraft) <$> accountDiscords)) $ readProp bowBotAccounts bdt
  case lookup did pns of
    Nothing -> pure NotOnList
    Just uuid -> do
      names <- liftIO $ fromMaybe [] <$> mcUUIDToNames manager bdt uuid
      res <- fun uuid names
      case res of
        Left e -> pure (UserError e)
        Right r -> pure (JustResponse (head names) r)

withMinecraftNormal :: MonadIO m => Manager -> BotData -> Bool -> String -> (String -> [String] -> m (Either e a)) -> m (MinecraftResponse e a)
withMinecraftNormal manager bdt cont mcname fun = do
  maybeUUID <- liftIO $ mcNameToUUID manager bdt mcname
  case maybeUUID of
    Nothing -> if cont then withMinecraftAutocorrect manager bdt False mcname fun else pure PlayerNotFound
    Just uuid -> do
      names <- liftIO $ fromMaybe [] <$> mcUUIDToNames manager bdt uuid
      res <- fun uuid names
      case res of
        Left e -> if cont then withMinecraftAutocorrect manager bdt False mcname fun else pure (UserError e)
        Right r -> pure (JustResponse (head names) r)

withMinecraftAutocorrect :: MonadIO m => Manager -> BotData -> Bool -> String -> (String -> [String] -> m (Either e a)) -> m (MinecraftResponse e a)
withMinecraftAutocorrect manager bdt cont mcname fun = do
  people <- readProp minecraftAccounts bdt
  let process f = let 
        nicks = [(mcUUID,u) | MinecraftAccount {..} <- people, u <- f mcNames]
        dists = map (\(u,n) -> ((u, n), dist (map toLower n) (map toLower mcname))) nicks
          in map fst . sortOn snd . filter (\(_,d) -> d <= 2) $ dists
  let filtered = process (take 1) ++ process (drop 1)
  case filtered of
    [] -> if cont then withMinecraftNormal manager bdt False mcname fun else pure PlayerNotFound
    ((uuid, rn):_) -> do
      names <- liftIO $ fromMaybe [] <$> mcUUIDToNames manager bdt uuid
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