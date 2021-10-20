{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module BowBot.Minecraft where

import Discord.Types
import Control.Monad.IO.Class (MonadIO, liftIO)
import BowBot.BotData
import Network.HTTP.Conduit (Manager)
import BowBot.API.Mojang
import Control.Concurrent.STM.TVar (readTVar)
import Control.Concurrent.STM (atomically, STM)
import Data.Maybe (fromMaybe)
import BowBot.Utils
import Data.Char (toLower, isDigit)
import Data.List (sortOn, isPrefixOf, isSuffixOf)
import Text.Read (readMaybe)


data MinecraftResponse a
  = JustResponse String a
  | OldResponse String String a
  | NoResponse
  | NotOnList
  | DidYouMeanResponse String a
  | DidYouMeanOldResponse String String a

mcNameToUUID :: Manager -> BotData -> String -> IO (Maybe String)
mcNameToUUID manager bdt name = do
  mcAcc <- atomically $ readTVar (minecraftAccounts bdt)
  let goodAcc = filter ((==name) . head . mcNames) mcAcc
  case goodAcc of
    [MinecraftAccount {mcUUID}] -> return (Just mcUUID)
    _ -> mojangNameToUUID manager name

mcUUIDToNames :: Manager -> BotData -> String -> IO (Maybe [String])
mcUUIDToNames manager bdt uuid = do
  mcAcc <- atomically $ readTVar (minecraftAccounts bdt)
  let goodAcc = filter ((==uuid) . mcUUID) mcAcc
  case goodAcc of
    [MinecraftAccount {mcNames}] -> return (Just mcNames)
    _ -> mojangUUIDToNames manager uuid

fromPingDiscordUser :: String -> Maybe UserId
fromPingDiscordUser str | "<@" `isPrefixOf` str && ">" `isSuffixOf` str = readMaybe $ filter isDigit str
fromPingDiscordUser _ = Nothing

withMinecraft :: MonadIO m => Manager -> BotData -> Bool -> Either String UserId -> (String -> [String] -> m (Maybe a)) -> m (MinecraftResponse a)
withMinecraft manager bdt _ (Right author) fun = withMinecraftDiscord manager bdt author fun
withMinecraft manager bdt _ (Left (fromPingDiscordUser -> Just did)) fun = do
  ret <- withMinecraftDiscord manager bdt did fun
  return $ case ret of
    NotOnList -> NoResponse -- TODO: make more granular responses
    _ -> ret
withMinecraft manager bdt ac (Left mcname) fun = 
  if ac then withMinecraftAutocorrect manager bdt True mcname fun else withMinecraftNormal manager bdt True mcname fun

withMinecraftDiscord :: MonadIO m => Manager -> BotData -> UserId -> (String -> [String] -> m (Maybe a)) -> m (MinecraftResponse a)
withMinecraftDiscord manager bdt did fun = do
  pns <- fmap (>>=(\BowBotAccount {..} -> (,accountSelectedMinecraft) <$> accountDiscords)) $ liftIO $ atomically $ readTVar (bowBotAccounts bdt)
  case lookup did pns of
    Nothing -> pure NotOnList
    Just uuid -> do
      names <- liftIO $ fromMaybe [] <$> mcUUIDToNames manager bdt uuid
      res <- fun uuid names
      case res of
        Nothing -> pure NoResponse
        Just r -> pure (JustResponse (head names) r)

withMinecraftNormal :: MonadIO m => Manager -> BotData -> Bool -> String -> (String -> [String] -> m (Maybe a)) -> m (MinecraftResponse a)
withMinecraftNormal manager bdt cont mcname fun = do
  maybeUUID <- liftIO $ mcNameToUUID manager bdt mcname
  case maybeUUID of
    Nothing -> if cont then withMinecraftAutocorrect manager bdt False mcname fun else pure NoResponse
    Just uuid -> do
      names <- liftIO $ fromMaybe [] <$> mcUUIDToNames manager bdt uuid
      res <- fun uuid names
      case res of
        Nothing -> if cont then withMinecraftAutocorrect manager bdt False mcname fun else pure NoResponse
        Just r -> pure (JustResponse (head names) r)

flattenedMinecraftNicks :: BotData -> STM [(String, String)]
flattenedMinecraftNicks BotData {..} = do
  people <- readTVar minecraftAccounts
  let currentNicks = [(mcUUID,u) | MinecraftAccount {..} <- people, u <- take 1 mcNames]
  let restOfNicks = [(mcUUID,u) | MinecraftAccount {..} <- people, u <- drop 1 mcNames]
  return $ currentNicks ++ restOfNicks

-- TODO: distinguish between no uuid and "fun" failing
withMinecraftAutocorrect :: MonadIO m => Manager -> BotData -> Bool -> String -> (String -> [String] -> m (Maybe a)) -> m (MinecraftResponse a)
withMinecraftAutocorrect manager bdt cont mcname fun = do
  people <- liftIO $ atomically $ flattenedMinecraftNicks bdt
  let dists = map (\(u,n) -> ((u, n), dist (map toLower n) (map toLower mcname))) people
  let filtered = map fst . sortOn snd . filter (\(_,d) -> d <= 2) $ dists
  case filtered of
    [] -> if cont then withMinecraftNormal manager bdt False mcname fun else pure NoResponse
    ((uuid, rn):_) -> do
      names <- liftIO $ fromMaybe [] <$> mcUUIDToNames manager bdt uuid
      res <- fun uuid names
      case res of
        Nothing -> pure NoResponse
        Just r -> 
          if dist (map toLower $ head names) (map toLower mcname) <= 2
          then if map toLower mcname == map toLower rn
            then pure (JustResponse rn r)
            else pure (DidYouMeanResponse rn r)
          else if map toLower mcname == map toLower rn
            then pure (OldResponse rn (head names) r)
            else pure (DidYouMeanOldResponse rn (head names) r)