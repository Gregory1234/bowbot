{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuantifiedConstraints #-}

module BowBot.Hypixel.TimeStats where

import BowBot.Hypixel.Stats
import BowBot.Hypixel.Leaderboard
import Data.Default
import BowBot.Minecraft.Basic
import qualified Data.HashMap.Strict as HM
import BowBot.BotData.Cached
import BowBot.DB.Basic
import BowBot.Utils
import BowBot.Settings.Basic
import Data.Maybe (catMaybes, mapMaybe)

data StatsTimeRange = DailyStats | WeeklyStats | MonthlyStats deriving (Show, Eq)

data SStatsTimeRange (t :: StatsTimeRange) where
  SDailyStats :: SStatsTimeRange 'DailyStats
  SWeeklyStats :: SStatsTimeRange 'WeeklyStats
  SMonthlyStats :: SStatsTimeRange 'MonthlyStats

deriving instance Show (SStatsTimeRange t)
deriving instance Eq (SStatsTimeRange t)

sStatsTimeRangeGet :: SStatsTimeRange t -> StatsTimeRange
sStatsTimeRangeGet SDailyStats = DailyStats
sStatsTimeRangeGet SWeeklyStats = WeeklyStats
sStatsTimeRangeGet SMonthlyStats = MonthlyStats

instance Default (SStatsTimeRange 'DailyStats) where def = SDailyStats
instance Default (SStatsTimeRange 'WeeklyStats) where def = SWeeklyStats
instance Default (SStatsTimeRange 'MonthlyStats) where def = SMonthlyStats

data HypixelBowTimeStats (t :: StatsTimeRange) = HypixelBowTimeStats
  { bowTimeWins :: Integer,
    bowTimeLosses :: Integer
  } deriving (Show, Eq)

statsTimeRangeName :: StatsTimeRange -> String
statsTimeRangeName DailyStats = "Day"
statsTimeRangeName WeeklyStats = "Week"
statsTimeRangeName MonthlyStats = "Month"

hypixelBowStatsToTimeStats :: HypixelBowStats -> HypixelBowTimeStats t
hypixelBowStatsToTimeStats HypixelBowStats {..} = HypixelBowTimeStats { bowTimeWins = bowWins, bowTimeLosses = bowLosses }

hypixelBowLeaderboardToTimeStats :: HypixelBowLeaderboardEntry -> HypixelBowTimeStats t
hypixelBowLeaderboardToTimeStats HypixelBowLeaderboardEntry {..} = HypixelBowTimeStats { bowTimeWins = bowLbWins, bowTimeLosses = bowLbLosses }


instance (Default (SStatsTimeRange t)) => Cached (HypixelBowTimeStats t) where
  type CacheIndex (HypixelBowTimeStats t) = UUID
  refreshCache conn = do
    cache <- getCache @(HypixelBowTimeStats t)
    res :: [(String, Integer, Integer)] <- queryLog conn (replaceQuery "TIME" (statsTimeRangeName $ sStatsTimeRangeGet (def :: SStatsTimeRange t)) "SELECT `minecraft`, `lastTIMEWins`, `lastTIMELosses` FROM `statsDEV` WHERE `lastTIMEWins` >= 0 AND `lastTIMELosses` >= 0") ()
    let newValues = HM.fromList $ flip fmap res $ \(UUID -> uuid, bowTimeWins, bowTimeLosses) -> (uuid, HypixelBowTimeStats {..})
    liftIO $ atomically $ writeTVar cache newValues

showHypixelBowTimeStats :: forall t. Default (SStatsTimeRange t) => Settings -> HypixelBowStats -> HypixelBowTimeStats t -> String
showHypixelBowTimeStats Settings {..} HypixelBowStats {..} HypixelBowTimeStats {..} = unlines $ catMaybes
  [ onlyIf sWins
  $ " - *Bow Duels " ++ time ++ " Wins:* **"
  ++ show (bowWins - bowTimeWins)
  ++ "**"
  , onlyIf sLosses
  $ " - *Bow Duels " ++ time ++ " Losses:* **"
  ++ show (bowLosses - bowTimeLosses)
  ++ "**"
  , onlyIf (sense sWLR (bowWins - bowTimeWins + bowLosses - bowTimeLosses /= 0))
  $ " - *Bow Duels " ++ time ++ " Win/Loss Ratio:* **"
  ++ winLossRatio
  ++ "**"
  ]
  where
    time = timeStatsTypeShowName (sStatsTimeRangeGet (def :: SStatsTimeRange t))
    timeStatsTypeShowName DailyStats = "Daily"
    timeStatsTypeShowName WeeklyStats = "Weekly"
    timeStatsTypeShowName MonthlyStats = "Monthly"
    sense Always _ = True
    sense Never _ = False
    sense WhenSensible x = x
    onlyIf True a = Just a
    onlyIf False _ = Nothing
    winLossRatio = showWLR (bowWins - bowTimeWins) (bowLosses - bowTimeLosses)

showMaybeHypixelBowTimeStats :: forall t. Default (SStatsTimeRange t) => Settings -> HypixelBowStats -> Maybe (HypixelBowTimeStats t) -> String
showMaybeHypixelBowTimeStats _ _ Nothing = case def :: SStatsTimeRange t of
  SDailyStats -> "**Daily data isn't avaliable yet for this player! Wait until tomorrow!**"
  SWeeklyStats -> "**Weekly data isn't avaliable yet for this player! Wait until next week!**"
  SMonthlyStats -> "**Monthly data isn't avaliable yet for this player! Wait until next month!**"
showMaybeHypixelBowTimeStats s t (Just v) = showHypixelBowTimeStats s t v

instance (Default (SStatsTimeRange t)) => CachedStorable (HypixelBowTimeStats t) where
  storeInCacheIndexed accs = do
    cacheMap <- getCacheMap
    let toQueryParams (uuid, lbe) = if Just lbe == cacheMap HM.!? uuid then Nothing else Just (uuidString uuid, bowTimeWins lbe, bowTimeLosses lbe)
    let queryParams = mapMaybe toQueryParams accs
    success <- liftIO $ withDB $ \conn -> (>0) <$> executeManyLog conn (replaceQuery "TIME" (statsTimeRangeName $ sStatsTimeRangeGet (def :: SStatsTimeRange t)) "INSERT INTO `statsDEV` (`minecraft`, `lastTIMEWins`, `lastTIMELosses`) VALUES (?,?,?) ON DUPLICATE KEY UPDATE `lastTIMEWins`=VALUES(`lastTIMEWins`), `lastTIMELosses`=VALUES(`lastTIMELosses`)") queryParams
    when success $ do
      cache <- getCache
      liftIO $ atomically $ modifyTVar cache (insertMany accs)
    return success

updateHypixelTimeStatsCache :: forall t m. (Default (SStatsTimeRange t), MonadCache HypixelBowLeaderboardEntry m, MonadCache (HypixelBowTimeStats t) m) => SStatsTimeRange t -> m ()
updateHypixelTimeStatsCache _ = do
  lbCache <- HM.toList <$> getCacheMap
  void $ storeInCacheIndexed (map (\(u,v) -> (u, hypixelBowLeaderboardToTimeStats @t v)) lbCache)

updateHypixelTimeStatsCache' :: forall m. (forall t. Default (SStatsTimeRange t) => MonadCache (HypixelBowTimeStats t) m, MonadCache HypixelBowLeaderboardEntry m) => StatsTimeRange -> m ()
updateHypixelTimeStatsCache' DailyStats = updateHypixelTimeStatsCache SDailyStats
updateHypixelTimeStatsCache' WeeklyStats = updateHypixelTimeStatsCache SWeeklyStats
updateHypixelTimeStatsCache' MonthlyStats = updateHypixelTimeStatsCache SMonthlyStats