{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BowBot.Command.Watchlist where

import BowBot.Command
import BowBot.Minecraft
import Data.Maybe (catMaybes)
import Control.Monad (filterM)
import Control.Concurrent.STM.TVar (newTVar)
import BowBot.API.Hypixel

listCommand :: Command
listCommand = Command "list" DefaultLevel 2 $ do -- TODO: add other lists
  st <- fromMaybe [] <$> getWatchlist
  people <- map head . catMaybes <$> traverse mcUUIDToNames st
  hRespond $ "**Players in watchList:**\n```\n" ++ unwords people ++ "```"
  pure ()

onlineCommand :: Command
onlineCommand = Command "online" DefaultLevel 30 $ do
  maybeOnlinePlayers <- hCache hypixelBowOnlineList $ do
    st <- fromMaybe [] <$> getWatchlist
    ret <- stm $ newTVar Nothing
    hTryApiRequests hypixelRequestCounter (length st) (\sec -> hRespond $ "**Too many requests! Wait another " ++ show sec ++ " seconds!**") $ do
      values <- filterM (fmap (fromMaybe False) . isInBowDuels) st
      stm $ writeTVar ret (Just values)
    stm $ readTVar ret
  case maybeOnlinePlayers of
    CacheFailed -> pure ()
    CacheBusy -> hRespond "**Processing list of online players. Please send command again later.**"
    CacheFresh v -> do
      online <- showOnline v
      hRespond $ "**Players in watchList currently in bow duels:**```\n" ++ online ++ "```"
    CacheOld v ->  do
      online <- showOnline v
      hRespond $ "**Players in watchList currently in bow duels:** (cached response)```\n" ++ online ++ "```"
  where
    showOnline [] = return "None of the watchListed players are currently in bow duels."
    showOnline uuids = do
      names <- map head . catMaybes <$> traverse mcUUIDToNames uuids
      return $ unlines . map (" - " ++) $ names

isInBowDuels :: APIMonad m => UUID -> m (Maybe Bool)
isInBowDuels uuid = hypixelWithPlayerStatus uuid $ \o -> do
    session <- o .: "session"
    mode :: String <- session .: "mode"
    return $ mode == "DUELS_BOW_DUEL"


getWatchlist :: APIMonad m => m (Maybe [UUID])
getWatchlist = do
  res <- hSendDB "minecraft/watchlist.php" []
  decodeParse res $ \o -> fmap UUID <$> o .: "data"