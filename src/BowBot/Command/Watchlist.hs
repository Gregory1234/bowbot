{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BowBot.Command.Watchlist where

import BowBot.Command
import BowBot.Minecraft
import Data.Maybe (catMaybes)
import BowBot.API
import Control.Monad (filterM)
import Control.Concurrent.STM.TVar (newTVar)
import BowBot.API.Hypixel

listCommand :: Command
listCommand = Command "list" DefaultLevel 2 $ \m man bdt -> do -- TODO: add other lists
  st <- liftIO $ fromMaybe [] <$> getWatchlist man
  people <- map head . catMaybes <$> traverse (liftIO . mcUUIDToNames man bdt) st
  respond m $ "**Players in watchList:**\n```\n" ++ unwords people ++ "```"
  pure ()

onlineCommand :: Command
onlineCommand = Command "online" DefaultLevel 30 $ \m man bdt -> do
  maybeOnlinePlayers <- getOrCalculateCache (hypixelBowOnlineList bdt) $ do
    st <- liftIO $ fromMaybe [] <$> getWatchlist man
    ret <- stm $ newTVar Nothing
    tryApiRequests (hypixelRequestCounter bdt) (length st) (\sec -> respond m $ "**Too many requests! Wait another " ++ show sec ++ " seconds!**") $ do
      values <- filterM (\uuid -> fromMaybe False <$> liftIO (isInBowDuels man uuid)) st
      stm $ writeTVar ret (Just values)
    stm $ readTVar ret
  case maybeOnlinePlayers of
    CacheFailed -> pure ()
    CacheBusy -> respond m "**Processing list of online players. Please send command again later.**"
    CacheFresh v -> do
      online <- showOnline man bdt v
      respond m $ "**Players in watchList currently in bow duels:**```\n" ++ online ++ "```"
    CacheOld v ->  do
      online <- showOnline man bdt v
      respond m $ "**Players in watchList currently in bow duels:** (cached response)```\n" ++ online ++ "```"
  where
    showOnline _ _ [] = return "None of the watchListed players are currently in bow duels."
    showOnline man bdt uuids = do
      names <- map head . catMaybes <$> traverse (liftIO . mcUUIDToNames man bdt) uuids
      return $ unlines . map (" - " ++) $ names

isInBowDuels :: Manager -> String -> IO (Maybe Bool)
isInBowDuels manager uuid = hypixelWithPlayerStatus manager uuid $ \o -> do
    session <- o .: "session"
    mode :: String <- session .: "mode"
    return $ mode == "DUELS_BOW_DUEL"


getWatchlist :: Manager -> IO (Maybe [String])
getWatchlist manager = do
  res <- sendDB manager "minecraft/watchlist.php" []
  decodeParse res $ \o -> o .: "data"