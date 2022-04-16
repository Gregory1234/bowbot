{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

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