{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module BowBot.Hypixel.Leaderboard where

import BowBot.Hypixel.Stats
import BowBot.BotData.Cached
import qualified Data.HashMap.Strict as HM
import BowBot.Minecraft.Basic (UUID(..))
import BowBot.DB.Basic
import BowBot.Utils
import BowBot.Hypixel.Basic (HypixelApi(..))
import BowBot.Network.Basic
import BowBot.Counter.Basic
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Data.Time.Clock (UTCTime(..))
import Database.MySQL.Simple.QueryParams (QueryParams(..))
import Database.MySQL.Simple.QueryResults (QueryResults(..))

data HypixelBowLeaderboardEntry = HypixelBowLeaderboardEntry
  { bowLbWins :: !Integer,
    bowLbLosses :: !Integer,
    bowLbWinstreak :: !(Maybe Integer),
    bowLbTimestamp :: !(Maybe UTCTime),
    bowLbWinstreakTimestamp :: !(Maybe UTCTime)
  } deriving (Show, Eq)

instance QueryParams HypixelBowLeaderboardEntry where
  renderParams HypixelBowLeaderboardEntry {..} = renderParams (bowLbWins, bowLbLosses, bowLbWinstreak, bowLbTimestamp, bowLbWinstreakTimestamp)
instance QueryResults HypixelBowLeaderboardEntry where
  convertResults fields strings = let
    (bowLbWins, bowLbLosses, (\x -> if x == 0 then Nothing else Just x) -> bowLbWinstreak, nullZeroTime -> bowLbTimestamp, nullZeroTime -> bowLbWinstreakTimestamp) = convertResults fields strings
      in HypixelBowLeaderboardEntry {..}
instance QueryResultsSize HypixelBowLeaderboardEntry where
  queryResultsSize _ = 5

hypixelBowStatsToLeaderboards :: HypixelBowStats -> HypixelBowLeaderboardEntry
hypixelBowStatsToLeaderboards HypixelBowStats {..} = HypixelBowLeaderboardEntry
  { bowLbWins = bowWins, bowLbLosses = bowLosses, bowLbWinstreak = cachedToMaybe bestWinstreak, bowLbTimestamp = bowStatsTimestamp, bowLbWinstreakTimestamp = cachedTimestamp bowStatsTimestamp bestWinstreak }

completeHypixelBowStats :: HypixelBowStats -> Maybe HypixelBowLeaderboardEntry -> HypixelBowStats
completeHypixelBowStats s Nothing = s
completeHypixelBowStats s (Just HypixelBowLeaderboardEntry {..}) = s { bestWinstreak = completeCachedMaybe bowLbWinstreakTimestamp (bestWinstreak s) bowLbWinstreak }

getHypixelBowLeaderboards :: (MonadIOReader m r, HasAll '[Connection] r) => m (HM.HashMap UUID HypixelBowLeaderboardEntry)
getHypixelBowLeaderboards = HM.fromList . map (\(Concat (Only a,b)) -> (a,b)) <$> queryLog "SELECT `minecraft`, `bowWins`, `bowLosses`, `bowWinstreak`, `lastUpdate`, `lastWinstreakUpdate` FROM `stats`" ()

updateHypixelBowLeaderboards :: (MonadIOReader m r, HasAll '[Manager, CounterState, Connection] r) => m ()
updateHypixelBowLeaderboards = do
  ctx <- ask
  let helper (uuid, old) = do
        stats <- fmap hypixelBowStatsToLeaderboards <$> requestHypixelBowStats uuid
        return (uuid, maybe old (\s -> s { bowLbWinstreak = bowLbWinstreak s <|> bowLbWinstreak old }) stats)
  leaderboard <- getHypixelBowLeaderboards
  let bigchunked = chunksOf 50 $ HM.toList leaderboard
  sequence_ $ intersperse (liftIO $ logInfoFork "Started 1 minute wait in Hypixel lb update" >> threadDelay 60000000) $ flip map bigchunked $ \bigchunk -> do
    let chunked = chunksOf 10 bigchunk
    let wait = do
          time <- tryIncreaseCounter HypixelApi 10
          case time of
            Nothing -> pure ()
            Just t -> do
              liftIO $ threadDelay ((t+1) * 1000000)
              wait
    wait
    newVals <- fmap concat $ liftIO $ for chunked $ \chunk -> mapConcurrently (fmap (`runReaderT` ctx) helper) chunk
    liftIO $ (`runReaderT` ctx) $ withTransaction $ do
      currentKeys :: [UUID] <- map fromOnly <$> queryLog "SELECT `minecraft` FROM `stats`" ()
      let queryParams = map (\(uuid, entry) -> Concat (Only uuid, entry)) $ filter (\(uuid, _) -> uuid `elem` currentKeys) newVals
      void $ executeManyLog "INSERT INTO `stats` (`minecraft`, `bowWins`, `bowLosses`, `bowWinstreak`, `lastUpdate`, `lastWinstreakUpdate`) VALUES (?,?,?,?,?,?) ON DUPLICATE KEY UPDATE `bowWins`=VALUES(`bowWins`), `bowLosses`=VALUES(`bowLosses`), `bowWinstreak`=VALUES(`bowWinstreak`), `lastUpdate`=VALUES(`lastUpdate`), `lastWinstreakUpdate`=VALUES(`lastWinstreakUpdate`)" queryParams

getHypixelBowLeaderboardEntryByUUID :: (MonadIOReader m r, Has Connection r) => UUID -> m (Maybe HypixelBowLeaderboardEntry)
getHypixelBowLeaderboardEntryByUUID uuid = only <$> queryLog "SELECT `bowWins`, `bowLosses`, `bowWinstreak`, `lastUpdate`, `lastWinstreakUpdate` FROM `stats` WHERE `minecraft` = ?" (Only uuid)

setHypixelBowLeaderboardEntryByUUID :: (MonadIOReader m r, Has Connection r) => UUID -> HypixelBowLeaderboardEntry -> m Bool
setHypixelBowLeaderboardEntryByUUID uuid entry = (>0) <$> executeLog "INSERT INTO `stats` (`minecraft`, `bowWins`, `bowLosses`, `bowWinstreak`, `lastUpdate`, `lastWinstreakUpdate`) VALUES (?,?,?,?,?,?) ON DUPLICATE KEY UPDATE `bowWins`=VALUES(`bowWins`), `bowLosses`=VALUES(`bowLosses`), `bowWinstreak`=VALUES(`bowWinstreak`), `lastUpdate`=VALUES(`lastUpdate`), `lastWinstreakUpdate`=VALUES(`lastWinstreakUpdate`)" (Concat (Only uuid, entry))

removeHypixelBowLeaderboardEntryByUUID :: (MonadIOReader m r, Has Connection r) => UUID -> m Bool
removeHypixelBowLeaderboardEntryByUUID uuid = (>0) <$> executeLog "DELETE FROM `stats` WHERE `minecraft` = ?" (Only uuid)
