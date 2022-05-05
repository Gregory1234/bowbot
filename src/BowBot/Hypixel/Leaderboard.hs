{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module BowBot.Hypixel.Leaderboard where

import BowBot.Hypixel.Stats
import BowBot.BotData.Cached
import qualified Data.HashMap.Strict as HM
import BowBot.Minecraft.Basic (UUID(..))
import BowBot.DB.Basic (queryLog, executeManyLog, withDB, logInfo)
import BowBot.Utils
import Data.Maybe (mapMaybe)
import Control.Applicative ((<|>))
import BowBot.Hypixel.Basic (HypixelApi)
import BowBot.Network.Class
import BowBot.BotData.Counter
import Data.List.Split (chunksOf)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import BowBot.Network.Monad (runNetworkT)
import Data.List (intersperse)

data HypixelBowLeaderboardEntry = HypixelBowLeaderboardEntry
  { bowLbWins :: Integer,
    bowLbLosses :: Integer,
    bowLbWinstreak :: Maybe Integer
  } deriving (Show, Eq)

instance Cached HypixelBowLeaderboardEntry where
  type CacheIndex HypixelBowLeaderboardEntry = UUID
  refreshCache conn = do
    cache <- getCache
    res :: [(String, Integer, Integer, Integer)] <- queryLog conn "SELECT `minecraft`, `bowWins`, `bowLosses`, `bowWinstreak` FROM `statsDEV`" ()
    let newValues = HM.fromList $ flip fmap res $ \case
          (UUID -> uuid, bowLbWins, bowLbLosses, (\x -> if x == 0 then Nothing else Just x) -> bowLbWinstreak) -> (uuid, HypixelBowLeaderboardEntry {..})
    liftIO $ atomically $ writeTVar cache newValues

hypixelBowStatsToLeaderboards :: HypixelBowStats -> HypixelBowLeaderboardEntry
hypixelBowStatsToLeaderboards HypixelBowStats {..} = HypixelBowLeaderboardEntry
  { bowLbWins = bowWins, bowLbLosses = bowLosses, bowLbWinstreak = bestWinstreak }

instance CachedStorable HypixelBowLeaderboardEntry where
  storeInCacheIndexed accs = do
    cacheMap <- getCacheMap
    let fixed = map (\(uuid, lbe) -> (uuid, let old = cacheMap HM.!? uuid; winstreak = bowLbWinstreak lbe <|> (bowLbWinstreak =<< old) in lbe { bowLbWinstreak = winstreak })) accs
    let toQueryParams (uuid, lbe) = if Just lbe == cacheMap HM.!? uuid then Nothing else Just (uuidString uuid, bowLbWins lbe, bowLbLosses lbe, fromMaybe 0 $ bowLbWinstreak lbe)
    let queryParams = mapMaybe toQueryParams fixed
    success <- liftIO $ withDB $ \conn -> (>0) <$> executeManyLog conn "INSERT INTO `statsDEV` (`minecraft`, `bowWins`, `bowLosses`, `bowWinstreak`) VALUES (?,?,?,?) ON DUPLICATE KEY UPDATE `bowWins`=VALUES(`bowWins`), `bowLosses`=VALUES(`bowLosses`), `bowWinstreak`=VALUES(`bowWinstreak`)" queryParams
    when success $ do
      cache <- getCache
      liftIO $ atomically $ modifyTVar cache (insertMany fixed)
    return success

class (MonadNetwork m, MonadCounter HypixelApi m) => CacheUpdateSourceConstraintForHypixelBowLeaderboardEntry m where
instance (MonadNetwork m, MonadCounter HypixelApi m) => CacheUpdateSourceConstraintForHypixelBowLeaderboardEntry m where

instance CachedUpdatable HypixelBowLeaderboardEntry where
  type CacheUpdateSourceConstraint HypixelBowLeaderboardEntry = CacheUpdateSourceConstraintForHypixelBowLeaderboardEntry
  updateCache = do
    manager <- hManager
    let helper (uuid, old) = do
          stats <- fmap hypixelBowStatsToLeaderboards <$> requestHypixelBowStats uuid
          return (uuid, maybe old (\s -> s { bowLbWinstreak = bowLbWinstreak s <|> bowLbWinstreak old }) stats)
    cache <- HM.toList <$> getCacheMap
    let bigchunked = chunksOf 50 cache
    updatedAccounts <- fmap concat $ sequence $ intersperse (([] <$) $ liftIO $ logInfo "Started 1 minute wait in Hypixel lb update" >> threadDelay 60000000) $ flip map bigchunked $ \bigchunk -> do
      let chunked = chunksOf 10 bigchunk
      let wait = do
            time <- tryIncreaseCounter @HypixelApi 10
            case time of
              Nothing -> pure ()
              Just t -> do
                liftIO $ threadDelay ((t+1) * 1000000)
                wait
      wait
      liftIO $ fmap concat $ for chunked $ \chunk -> mapConcurrently (fmap (`runNetworkT` manager) helper) chunk
    void $ storeInCacheIndexed updatedAccounts
