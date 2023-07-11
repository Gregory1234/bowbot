{-# LANGUAGE TypeFamilies #-}

module BowBot.Hypixel.TimeStats(
  module BowBot.Hypixel.TimeStats, module BowBot.Stats.TimeRange
) where

import BowBot.Hypixel.Stats
import BowBot.Hypixel.Leaderboard
import BowBot.Minecraft.Basic
import BowBot.DB.Typed
import BowBot.Utils
import BowBot.Settings.Basic
import BowBot.Discord.Utils
import qualified Data.Text as T
import BowBot.Hypixel.Stats.Table
import BowBot.Hypixel.TimeStats.Table
import BowBot.Stats.TimeRange

data HypixelBowTimeStats = HypixelBowTimeStats
  { bowTimeWins :: Integer,
    bowTimeLosses :: Integer,
    bowTimeTimestamp :: Maybe UTCTime
  } deriving (Show, Eq)

instance QueryParams HypixelBowTimeStats where
  renderParams HypixelBowTimeStats {..} = [render bowTimeWins, render bowTimeLosses, render bowTimeTimestamp]
instance QueryResults HypixelBowTimeStats where
  convertResults = HypixelBowTimeStats <$> convert <*> convert <*> fmap nullZeroTime convert
instance InTable TimeStatsTable HypixelBowTimeStats where
  columnRep = ColRep [SomeCol TimeStatsTWins, SomeCol TimeStatsTLosses, SomeCol TimeStatsTLastUpdate]
instance InTable StatsTable HypixelBowTimeStats where
  columnRep = ColRep [SomeCol StatsTWins, SomeCol StatsTLosses, SomeCol StatsTLastUpdate]

hypixelBowStatsToTimeStats :: HypixelBowStats -> HypixelBowTimeStats
hypixelBowStatsToTimeStats HypixelBowStats {..} = HypixelBowTimeStats { bowTimeWins = bowWins, bowTimeLosses = bowLosses, bowTimeTimestamp = bowStatsTimestamp }

hypixelBowLeaderboardToTimeStats :: HypixelBowLeaderboardEntry -> HypixelBowTimeStats
hypixelBowLeaderboardToTimeStats HypixelBowLeaderboardEntry {..} = HypixelBowTimeStats { bowTimeWins = bowLbWins, bowTimeLosses = bowLbLosses, bowTimeTimestamp = bowLbTimestamp }

showHypixelBowTimeStats :: TimeRange -> Settings -> HypixelBowStats -> HypixelBowTimeStats -> Text
showHypixelBowTimeStats timeRange Settings {..} HypixelBowStats {..} HypixelBowTimeStats {..} = T.unlines $ catMaybes
  [ ("- *Since:* " <>) . discordFormatTimestampFull <$> bowTimeTimestamp
  , onlyIfBin sWins
  $ " - *Bow Duels " <> time <> " Wins:* **"
  <> showt (bowWins - bowTimeWins)
  <> "**"
  , onlyIfBin sLosses
  $ " - *Bow Duels " <> time <> " Losses:* **"
  <> showt (bowLosses - bowTimeLosses)
  <> "**"
  , onlyIfTer sWLR (bowWins - bowTimeWins + bowLosses - bowTimeLosses /= 0)
  $ " - *Bow Duels " <> time <> " Win/Loss Ratio:* **"
  <> winLossRatio
  <> "**"
  ]
  where
    time = timeStatsTypeShowName timeRange
    timeStatsTypeShowName DailyStats = "Daily"
    timeStatsTypeShowName WeeklyStats = "Weekly"
    timeStatsTypeShowName MonthlyStats = "Monthly"
    winLossRatio = showWLR (WLR (bowWins - bowTimeWins) (bowLosses - bowTimeLosses))

showMaybeHypixelBowTimeStats :: TimeRange -> Settings -> HypixelBowStats -> Maybe HypixelBowTimeStats -> Text
showMaybeHypixelBowTimeStats DailyStats _ _ Nothing = "- **Daily data isn't avaliable yet for this player! Wait until tomorrow!**"
showMaybeHypixelBowTimeStats WeeklyStats _ _ Nothing = "- **Weekly data isn't avaliable yet for this player! Wait until next week!**"
showMaybeHypixelBowTimeStats MonthlyStats _ _ Nothing = "- **Monthly data isn't avaliable yet for this player! Wait until next month!**"
showMaybeHypixelBowTimeStats tm s t (Just v) = showHypixelBowTimeStats tm s t v

updateHypixelBowTimeStats :: (MonadIOReader m r, Has Connection r) => TimeRange -> m ()
updateHypixelBowTimeStats time = void $ executeLogT (insertGenQuery 
  (ColRep @TimeStatsTable @((UUID, TimeRange), HypixelBowTimeStats) $ [SomeCol TimeStatsTUUID, SomeCol TimeStatsTTime] ++ someCols @TimeStatsTable @HypixelBowTimeStats) 
  (selectSingleGenQuery (AndSelector2 (AndSelector2 (ColSelector StatsTUUID) ConstSelector) (EntSelector @HypixelBowTimeStats columnRep)) NoCondition)) time

getHypixelBowTimeStatsByUUID :: (MonadIOReader m r, Has Connection r) => TimeRange -> UUID -> m (Maybe HypixelBowTimeStats)
getHypixelBowTimeStatsByUUID time uuid = queryOnlyLogT selectByPrimaryQuery (TimeStatsTPrimary uuid time)

data FullHypixelBowTimeStats = FullHypixelBowTimeStats
  { currentHypixelBowStats :: HypixelBowStats
  , dailyHypixelBowStats :: Maybe HypixelBowTimeStats
  , weeklyHypixelBowStats :: Maybe HypixelBowTimeStats
  , monthlyHypixelBowStats :: Maybe HypixelBowTimeStats
  }

showFullHypixelBowTimeStats :: Settings -> FullHypixelBowTimeStats -> Text
showFullHypixelBowTimeStats settings FullHypixelBowTimeStats {..} = 
     showMaybeHypixelBowTimeStats DailyStats settings currentHypixelBowStats dailyHypixelBowStats <> "\n" 
  <> showMaybeHypixelBowTimeStats WeeklyStats settings currentHypixelBowStats weeklyHypixelBowStats <> "\n" 
  <> showMaybeHypixelBowTimeStats MonthlyStats settings currentHypixelBowStats monthlyHypixelBowStats