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
import BowBot.Discord.Utils
import qualified Data.Text as T

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
    bowTimeLosses :: Integer,
    bowTimeTimestamp :: Maybe UTCTime
  } deriving (Show, Eq)

statsTimeRangeName :: StatsTimeRange -> Text
statsTimeRangeName DailyStats = "Day"
statsTimeRangeName WeeklyStats = "Week"
statsTimeRangeName MonthlyStats = "Month"

hypixelBowStatsToTimeStats :: HypixelBowStats -> HypixelBowTimeStats t
hypixelBowStatsToTimeStats HypixelBowStats {..} = HypixelBowTimeStats { bowTimeWins = bowWins, bowTimeLosses = bowLosses, bowTimeTimestamp = bowStatsTimestamp }

hypixelBowLeaderboardToTimeStats :: HypixelBowLeaderboardEntry -> HypixelBowTimeStats t
hypixelBowLeaderboardToTimeStats HypixelBowLeaderboardEntry {..} = HypixelBowTimeStats { bowTimeWins = bowLbWins, bowTimeLosses = bowLbLosses, bowTimeTimestamp = bowLbTimestamp }


instance (Default (SStatsTimeRange t)) => Cached (HypixelBowTimeStats t) where
  type CacheIndex (HypixelBowTimeStats t) = UUID
  refreshCache = do
    cache <- getCache @(HypixelBowTimeStats t)
    res :: [(Text, Integer, Integer, UTCTime)] <- queryLog (replaceQuery "TIME" (statsTimeRangeName $ sStatsTimeRangeGet (def :: SStatsTimeRange t)) "SELECT `minecraft`, `lastTIMEWins`, `lastTIMELosses`, `lastTIMEUpdate` FROM `stats` WHERE `lastTIMEWins` >= 0 AND `lastTIMELosses` >= 0") ()
    let newValues = HM.fromList $ flip fmap res $ \(UUID -> uuid, bowTimeWins, bowTimeLosses, nullZeroTime -> bowTimeTimestamp) -> (uuid, HypixelBowTimeStats {..})
    liftIO $ atomically $ writeTVar cache newValues

showHypixelBowTimeStats :: forall t. Default (SStatsTimeRange t) => Settings -> HypixelBowStats -> HypixelBowTimeStats t -> Text
showHypixelBowTimeStats Settings {..} HypixelBowStats {..} HypixelBowTimeStats {..} = T.unlines $ catMaybes
  [ ("*Since:* " <>) . discordFormatTimestampFull <$> bowTimeTimestamp
  , onlyIf sWins
  $ " - *Bow Duels " <> time <> " Wins:* **"
  <> pack (show (bowWins - bowTimeWins))
  <> "**"
  , onlyIf sLosses
  $ " - *Bow Duels " <> time <> " Losses:* **"
  <> pack (show (bowLosses - bowTimeLosses))
  <> "**"
  , onlyIf (sense sWLR (bowWins - bowTimeWins + bowLosses - bowTimeLosses /= 0))
  $ " - *Bow Duels " <> time <> " Win/Loss Ratio:* **"
  <> winLossRatio
  <> "**"
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

showMaybeHypixelBowTimeStats :: forall t. Default (SStatsTimeRange t) => Settings -> HypixelBowStats -> Maybe (HypixelBowTimeStats t) -> Text
showMaybeHypixelBowTimeStats _ _ Nothing = case def :: SStatsTimeRange t of
  SDailyStats -> "**Daily data isn't avaliable yet for this player! Wait until tomorrow!**"
  SWeeklyStats -> "**Weekly data isn't avaliable yet for this player! Wait until next week!**"
  SMonthlyStats -> "**Monthly data isn't avaliable yet for this player! Wait until next month!**"
showMaybeHypixelBowTimeStats s t (Just v) = showHypixelBowTimeStats s t v

instance (Default (SStatsTimeRange t)) => CachedStorable (HypixelBowTimeStats t) where
  storeInCacheIndexed accs = do
    cacheMap <- getCacheMap
    let toQueryParams (uuid, lbe) = if Just lbe == cacheMap HM.!? uuid then Nothing else Just (uuidString uuid, bowTimeWins lbe, bowTimeLosses lbe, unNullZeroTime $ bowTimeTimestamp lbe)
    let queryParams = mapMaybe toQueryParams accs
    success <- liftIO $ withDB $ \conn -> (>0) <$> executeManyLog' conn (replaceQuery "TIME" (statsTimeRangeName $ sStatsTimeRangeGet (def :: SStatsTimeRange t)) "INSERT INTO `stats` (`minecraft`, `lastTIMEWins`, `lastTIMELosses`, `lastTIMEUpdate`) VALUES (?,?,?,?) ON DUPLICATE KEY UPDATE `lastTIMEWins`=VALUES(`lastTIMEWins`), `lastTIMELosses`=VALUES(`lastTIMELosses`), `lastTIMEUpdate`=VALUES(`lastTIMEUpdate`)") queryParams
    when success $ do
      cache <- getCache
      liftIO $ atomically $ modifyTVar cache (insertMany accs)
    return success

updateHypixelTimeStatsCache :: forall t m d r. (Default (SStatsTimeRange t), MonadIOBotData m d r, HasCaches [HypixelBowLeaderboardEntry,HypixelBowTimeStats t] d) => SStatsTimeRange t -> m ()
updateHypixelTimeStatsCache _ = do
  lbCache <- HM.toList <$> getCacheMap
  void $ storeInCacheIndexed (map (\(u,v) -> (u, hypixelBowLeaderboardToTimeStats @t v)) lbCache)

updateHypixelTimeStatsCache' :: forall m d r. (forall t. Default (SStatsTimeRange t) => HasCache (HypixelBowTimeStats t) d, MonadIOBotData m d r, HasCache HypixelBowLeaderboardEntry d) => StatsTimeRange -> m ()
updateHypixelTimeStatsCache' DailyStats = updateHypixelTimeStatsCache SDailyStats
updateHypixelTimeStatsCache' WeeklyStats = updateHypixelTimeStatsCache SWeeklyStats
updateHypixelTimeStatsCache' MonthlyStats = updateHypixelTimeStatsCache SMonthlyStats