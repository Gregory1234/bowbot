{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

module BowBot.Hypixel.Leaderboard where

import BowBot.Hypixel.Stats
import BowBot.BotData.Cached
import qualified Data.HashMap.Strict as HM
import BowBot.Minecraft.Basic (UUID(..))
import BowBot.DB.Basic (queryLog, executeManyLog', withDB, logInfoFork)
import BowBot.Utils
import BowBot.Hypixel.Basic (HypixelApi(..))
import BowBot.Network.Basic
import BowBot.Counter.Basic
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Data.Time.Clock (UTCTime(..))

data HypixelBowLeaderboardEntry = HypixelBowLeaderboardEntry
  { bowLbWins :: Integer,
    bowLbLosses :: Integer,
    bowLbWinstreak :: Maybe Integer,
    bowLbTimestamp :: Maybe UTCTime,
    bowLbWinstreakTimestamp :: Maybe UTCTime
  } deriving (Show, Eq)

instance Cached HypixelBowLeaderboardEntry where
  type CacheIndex HypixelBowLeaderboardEntry = UUID
  refreshCache = do
    cache <- getCache
    res :: [(Text, Integer, Integer, Integer, UTCTime, UTCTime)] <- queryLog "SELECT `minecraft`, `bowWins`, `bowLosses`, `bowWinstreak`, `lastUpdate`, `lastWinstreakUpdate` FROM `stats`" ()
    let newValues = HM.fromList $ flip fmap res $ \case
          (UUID -> uuid, bowLbWins, bowLbLosses, (\x -> if x == 0 then Nothing else Just x) -> bowLbWinstreak, nullZeroTime -> bowLbTimestamp, nullZeroTime -> bowLbWinstreakTimestamp) -> (uuid, HypixelBowLeaderboardEntry {..})
    liftIO $ atomically $ writeTVar cache newValues

hypixelBowStatsToLeaderboards :: HypixelBowStats -> HypixelBowLeaderboardEntry
hypixelBowStatsToLeaderboards HypixelBowStats {..} = HypixelBowLeaderboardEntry
  { bowLbWins = bowWins, bowLbLosses = bowLosses, bowLbWinstreak = cachedToMaybe bestWinstreak, bowLbTimestamp = bowStatsTimestamp, bowLbWinstreakTimestamp = cachedTimestamp bowStatsTimestamp bestWinstreak }

instance CachedStorable HypixelBowLeaderboardEntry where
  storeInCacheIndexed accs = do
    cacheMap <- getCacheMap
    let fixed = map (\(uuid, lbe) -> (uuid, let old = cacheMap HM.!? uuid; winstreak = bowLbWinstreak lbe <|> (bowLbWinstreak =<< old); winstreakTimestamp = bowLbWinstreakTimestamp lbe <|> (bowLbWinstreakTimestamp =<< old) in lbe { bowLbWinstreak = winstreak, bowLbWinstreakTimestamp = winstreakTimestamp })) accs
    let toQueryParams (uuid, lbe) = if Just lbe == cacheMap HM.!? uuid then Nothing else Just (uuidString uuid, bowLbWins lbe, bowLbLosses lbe, fromMaybe 0 $ bowLbWinstreak lbe, unNullZeroTime $ bowLbTimestamp lbe, unNullZeroTime $ bowLbWinstreakTimestamp lbe)
    let queryParams = mapMaybe toQueryParams fixed
    success <- liftIO $ withDB $ \conn -> (>0) <$> executeManyLog' conn "INSERT INTO `stats` (`minecraft`, `bowWins`, `bowLosses`, `bowWinstreak`, `lastUpdate`, `lastWinstreakUpdate`) VALUES (?,?,?,?,?,?) ON DUPLICATE KEY UPDATE `bowWins`=VALUES(`bowWins`), `bowLosses`=VALUES(`bowLosses`), `bowWinstreak`=VALUES(`bowWinstreak`), `lastUpdate`=VALUES(`lastUpdate`), `lastWinstreakUpdate`=VALUES(`lastWinstreakUpdate`)" queryParams
    when success $ do
      cache <- getCache
      liftIO $ atomically $ modifyTVar cache (insertMany fixed)
    return success

updateHypixelLeaderboardCache :: (MonadIOBotData m d r, HasAll '[Manager, CounterState] r, HasCache HypixelBowLeaderboardEntry d) => m ()
updateHypixelLeaderboardCache = do
  ctx <- ask
  let helper (uuid, old) = do
        stats <- fmap hypixelBowStatsToLeaderboards <$> requestHypixelBowStats uuid
        return (uuid, maybe old (\s -> s { bowLbWinstreak = bowLbWinstreak s <|> bowLbWinstreak old }) stats)
  cache <- HM.toList <$> getCacheMap
  let bigchunked = chunksOf 50 cache
  updatedAccounts <- fmap concat $ sequence $ intersperse (([] <$) $ liftIO $ logInfoFork "Started 1 minute wait in Hypixel lb update" >> threadDelay 60000000) $ flip map bigchunked $ \bigchunk -> do
    let chunked = chunksOf 10 bigchunk
    let wait = do
          time <- tryIncreaseCounter HypixelApi 10
          case time of
            Nothing -> pure ()
            Just t -> do
              liftIO $ threadDelay ((t+1) * 1000000)
              wait
    wait
    liftIO $ fmap concat $ for chunked $ \chunk -> mapConcurrently (fmap (`runReaderT` ctx) helper) chunk
  void $ storeInCacheIndexed updatedAccounts

completeHypixelBowStats :: HypixelBowStats -> Maybe HypixelBowLeaderboardEntry -> HypixelBowStats
completeHypixelBowStats s Nothing = s
completeHypixelBowStats s (Just HypixelBowLeaderboardEntry {..}) = s { bestWinstreak = completeCachedMaybe bowLbWinstreakTimestamp (bestWinstreak s) bowLbWinstreak }