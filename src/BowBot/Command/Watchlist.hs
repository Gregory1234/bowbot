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
  bdt <- hData
  st <- fromMaybe [] <$> getWatchlist
  people <- map head . catMaybes <$> traverse (mcUUIDToNames bdt) st
  hRespond $ "**Players in watchList:**\n```\n" ++ unwords people ++ "```"
  pure ()

onlineCommand :: Command
onlineCommand = Command "online" DefaultLevel 30 $ do
  bdt <- hData
  maybeOnlinePlayers <- getOrCalculateCache (hypixelBowOnlineList bdt) $ do
    st <- fromMaybe [] <$> getWatchlist
    ret <- stm $ newTVar Nothing
    tryApiRequests (hypixelRequestCounter bdt) (length st) (\sec -> hRespond $ "**Too many requests! Wait another " ++ show sec ++ " seconds!**") $ do
      values <- filterM (fmap (fromMaybe False) . isInBowDuels) st
      stm $ writeTVar ret (Just values)
    stm $ readTVar ret
  case maybeOnlinePlayers of
    CacheFailed -> pure ()
    CacheBusy -> hRespond "**Processing list of online players. Please send command again later.**"
    CacheFresh v -> do
      online <- showOnline bdt v
      hRespond $ "**Players in watchList currently in bow duels:**```\n" ++ online ++ "```"
    CacheOld v ->  do
      online <- showOnline bdt v
      hRespond $ "**Players in watchList currently in bow duels:** (cached response)```\n" ++ online ++ "```"
  where
    showOnline _ [] = return "None of the watchListed players are currently in bow duels."
    showOnline bdt uuids = do
      names <- map head . catMaybes <$> traverse (mcUUIDToNames bdt) uuids
      return $ unlines . map (" - " ++) $ names

isInBowDuels :: APIMonad m => String -> m (Maybe Bool)
isInBowDuels uuid = hypixelWithPlayerStatus uuid $ \o -> do
    session <- o .: "session"
    mode :: String <- session .: "mode"
    return $ mode == "DUELS_BOW_DUEL"


getWatchlist :: APIMonad m => m (Maybe [String])
getWatchlist = do
  res <- hSendDB "minecraft/watchlist.php" []
  decodeParse res $ \o -> o .: "data"