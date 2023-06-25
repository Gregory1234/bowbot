module BowBot.Hypixel.Leaderboard where

import BowBot.Hypixel.Stats
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
import BowBot.Hypixel.LeaderboardStatus

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
    (bowLbWins, bowLbLosses, bowLbWinstreak, bowLbTimestamp, bowLbWinstreakTimestamp) = convertResults fields strings
      in HypixelBowLeaderboardEntry {..}
instance QueryResultsSize HypixelBowLeaderboardEntry where
  queryResultsSize _ = 5

bowLbWLR :: HypixelBowLeaderboardEntry -> WLR Integer
bowLbWLR HypixelBowLeaderboardEntry {..} = WLR bowLbWins bowLbLosses

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
  let helper uuid = fmap ((uuid,) . hypixelBowStatsToLeaderboards) <$> requestHypixelBowStats uuid
  toUpdate <- getHypixelUnbanned
  let bigchunked = chunksOf 50 toUpdate
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
    newVals <- fmap (catMaybes . concat) $ liftIO $ for chunked $ \chunk -> mapConcurrently (fmap (`runReaderT` ctx) helper) chunk
    liftIO $ (`runReaderT` ctx) $ withTransaction $ do
      let queryParams = map (\(uuid, entry) -> Concat (Only uuid, entry)) newVals
      void $ executeManyLog "INSERT INTO `stats` (`minecraft`, `bowWins`, `bowLosses`, `bowWinstreak`, `lastUpdate`, `lastWinstreakUpdate`) VALUES (?,?,?,?,?,?) ON DUPLICATE KEY UPDATE `bowWins`=VALUES(`bowWins`), `bowLosses`=VALUES(`bowLosses`), `bowWinstreak`=IFNULL(VALUES(`bowWinstreak`),`bowWinstreak`), `lastUpdate`=IFNULL(VALUES(`lastUpdate`),`lastUpdate`), `lastWinstreakUpdate`=IFNULL(VALUES(`lastWinstreakUpdate`),`lastWinstreakUpdate`)" queryParams

getHypixelBowLeaderboardEntryByUUID :: (MonadIOReader m r, Has Connection r) => UUID -> m (Maybe HypixelBowLeaderboardEntry)
getHypixelBowLeaderboardEntryByUUID uuid = only <$> queryLog "SELECT `bowWins`, `bowLosses`, `bowWinstreak`, `lastUpdate`, `lastWinstreakUpdate` FROM `stats` WHERE `minecraft` = ?" (Only uuid)

setHypixelBowLeaderboardEntryByUUID :: (MonadIOReader m r, Has Connection r) => UUID -> HypixelBowLeaderboardEntry -> m Bool
setHypixelBowLeaderboardEntryByUUID uuid entry = (>0) <$> executeLog "INSERT INTO `stats` (`minecraft`, `bowWins`, `bowLosses`, `bowWinstreak`, `lastUpdate`, `lastWinstreakUpdate`) VALUES (?,?,?,?,?,?) ON DUPLICATE KEY UPDATE `bowWins`=VALUES(`bowWins`), `bowLosses`=VALUES(`bowLosses`), `bowWinstreak`=IFNULL(VALUES(`bowWinstreak`),`bowWinstreak`), `lastUpdate`=IFNULL(VALUES(`lastUpdate`),`lastUpdate`), `lastWinstreakUpdate`=IFNULL(VALUES(`lastWinstreakUpdate`),`lastWinstreakUpdate`)" (Concat (Only uuid, entry))

removeHypixelBowLeaderboardEntryByUUID :: (MonadIOReader m r, Has Connection r) => UUID -> m Bool
removeHypixelBowLeaderboardEntryByUUID uuid = (>0) <$> executeLog "DELETE FROM `stats` WHERE `minecraft` = ?" (Only uuid)
