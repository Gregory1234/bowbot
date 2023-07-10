{-# LANGUAGE TypeFamilies #-}

module BowBot.Hypixel.Leaderboard where

import BowBot.Hypixel.Stats
import qualified Data.HashMap.Strict as HM
import BowBot.Minecraft.Basic (UUID(..))
import BowBot.DB.Typed
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
  renderParams HypixelBowLeaderboardEntry {..} = [render bowLbWins, render bowLbLosses, render bowLbWinstreak, render bowLbTimestamp, render bowLbWinstreakTimestamp]
instance QueryResults HypixelBowLeaderboardEntry where
  convertResults = HypixelBowLeaderboardEntry <$> convert <*> convert <*> convert <*> convert <*> convert
instance DatabaseTable HypixelBowLeaderboardEntry where
  type PrimaryKey HypixelBowLeaderboardEntry = UUID
  databaseTableName _ = "hypixel_bow_stats"
  databaseColumnNames _ = ["wins", "losses", "winstreak", "last_update", "last_winstreak_update"]
  databasePrimaryKey _ = ["minecraft_uuid"]

bowLbWLR :: HypixelBowLeaderboardEntry -> WLR Integer
bowLbWLR HypixelBowLeaderboardEntry {..} = WLR bowLbWins bowLbLosses

hypixelBowStatsToLeaderboards :: HypixelBowStats -> HypixelBowLeaderboardEntry
hypixelBowStatsToLeaderboards HypixelBowStats {..} = HypixelBowLeaderboardEntry
  { bowLbWins = bowWins, bowLbLosses = bowLosses, bowLbWinstreak = cachedToMaybe bestWinstreak, bowLbTimestamp = bowStatsTimestamp, bowLbWinstreakTimestamp = cachedTimestamp bowStatsTimestamp bestWinstreak }

completeHypixelBowStats :: HypixelBowStats -> Maybe HypixelBowLeaderboardEntry -> HypixelBowStats
completeHypixelBowStats s Nothing = s
completeHypixelBowStats s (Just HypixelBowLeaderboardEntry {..}) = s { bestWinstreak = completeCachedMaybe bowLbWinstreakTimestamp (bestWinstreak s) bowLbWinstreak }

getHypixelBowLeaderboards :: (MonadIOReader m r, HasAll '[Connection] r) => m (HM.HashMap UUID HypixelBowLeaderboardEntry)
getHypixelBowLeaderboards = HM.fromList . map (\(KeyedRow a b) -> (a,b)) <$> queryLogT_ selectAllQueryKeyed

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
      let queryParams = map (uncurry KeyedRow) newVals
      void $ executeManyLogT insertQueryKeyed queryParams

getHypixelBowLeaderboardEntryByUUID :: (MonadIOReader m r, Has Connection r) => UUID -> m (Maybe HypixelBowLeaderboardEntry)
getHypixelBowLeaderboardEntryByUUID = queryOnlyLogT selectByPrimaryQuery

setHypixelBowLeaderboardEntryByUUID :: (MonadIOReader m r, Has Connection r) => UUID -> HypixelBowLeaderboardEntry -> m Bool
setHypixelBowLeaderboardEntryByUUID uuid entry = (>0) <$> executeLogT insertQueryKeyed (KeyedRow uuid entry)

removeHypixelBowLeaderboardEntryByUUID :: (MonadIOReader m r, Has Connection r) => UUID -> m Bool
removeHypixelBowLeaderboardEntryByUUID uuid = (>0) <$> executeLog "DELETE FROM `hypixel_bow_stats` WHERE `minecraft` = ?" uuid
