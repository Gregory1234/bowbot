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
import Data.Proxy
import BowBot.Minecraft.Basic (UUID(..))
import BowBot.DB.Basic (queryLog, executeManyLog, withDB)
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

data HypixelBowLeaderboardEntry = HypixelBowLeaderboardEntry
  { bowLbWins :: Integer,
    bowLbLosses :: Integer,
    bowLbWinstreak :: Maybe Integer
  } deriving (Show, Eq)

instance Cached HypixelBowLeaderboardEntry where
  type CacheIndex HypixelBowLeaderboardEntry = UUID
  refreshCache conn _ = do
    cache <- getCache (Proxy @HypixelBowLeaderboardEntry)
    res :: [(String, Integer, Integer, Integer)] <- queryLog conn "SELECT `minecraft`, `bowWins`, `bowLosses`, `bowWinstreak` FROM `statsDEV`" ()
    let newValues = HM.fromList $ flip fmap res $ \case
          (UUID -> uuid, bowLbWins, bowLbLosses, (\x -> if x == 0 then Nothing else Just x) -> bowLbWinstreak) -> (uuid, HypixelBowLeaderboardEntry {..})
    liftIO $ atomically $ writeTVar cache newValues

hypixelBowStatsToLeaderboards :: HypixelBowStats -> HypixelBowLeaderboardEntry
hypixelBowStatsToLeaderboards HypixelBowStats {..} = HypixelBowLeaderboardEntry
  { bowLbWins = bowWins, bowLbLosses = bowLosses, bowLbWinstreak = bestWinstreak }

instance CachedStorable HypixelBowLeaderboardEntry where
  storeInCacheIndexed accs = do
    cacheMap <- getCacheMap (Proxy @HypixelBowLeaderboardEntry)
    let fixed = map (\(uuid, lbe) -> (uuid, let old = cacheMap HM.!? uuid; winstreak = bowLbWinstreak lbe <|> (bowLbWinstreak =<< old) in lbe { bowLbWinstreak = winstreak })) accs
    let toQueryParams (uuid, lbe) = if Just lbe == cacheMap HM.!? uuid then Nothing else Just (uuidString uuid, bowLbWins lbe, bowLbLosses lbe, fromMaybe 0 $ bowLbWinstreak lbe)
    let queryParams = mapMaybe toQueryParams fixed
    success <- liftIO $ withDB $ \conn -> (>0) <$> executeManyLog conn "INSERT INTO `statsDEV` (`minecraft`, `bowWins`, `bowLosses`, `bowWinstreak`) VALUES (?,?,?,?) ON DUPLICATE KEY UPDATE `bowWins`=VALUES(`bowWins`), `bowLosses`=VALUES(`bowLosses`), `bowWinstreak`=VALUES(`bowWinstreak`)" queryParams
    when success $ do
      cache <- getCache (Proxy @HypixelBowLeaderboardEntry)
      liftIO $ atomically $ modifyTVar cache (insertMany fixed)
    return success

class (MonadNetwork m, MonadCounter HypixelApi m) => CacheUpdateSourceConstraintForHypixelBowLeaderboardEntry m where
instance (MonadNetwork m, MonadCounter HypixelApi m) => CacheUpdateSourceConstraintForHypixelBowLeaderboardEntry m where

instance CachedUpdatable HypixelBowLeaderboardEntry where
  type CacheUpdateSourceConstraint HypixelBowLeaderboardEntry = CacheUpdateSourceConstraintForHypixelBowLeaderboardEntry
  updateCache proxy = do
    manager <- hManager
    let helper (uuid, old) = do
          stats <- fmap hypixelBowStatsToLeaderboards <$> requestHypixelBowStats uuid
          return (uuid, maybe old (\s -> s { bowLbWinstreak = bowLbWinstreak s <|> bowLbWinstreak old }) stats)
    cache <- HM.toList <$> getCacheMap proxy
    let chunked = chunksOf 30 cache
    updatedAccounts <- fmap concat $ for chunked $ \chunk ->
      let tryUpdate = do
            time <- tryIncreaseCounter (Proxy @HypixelApi) 30
            case time of
              Nothing -> do
                ret <- liftIO $ mapConcurrently (fmap (`runNetworkT` manager) helper) chunk
                liftIO $ threadDelay 20000000
                return ret
              Just t -> do
                liftIO $ threadDelay (t * 1000000)
                tryUpdate
      in tryUpdate
    void $ storeInCacheIndexed updatedAccounts
