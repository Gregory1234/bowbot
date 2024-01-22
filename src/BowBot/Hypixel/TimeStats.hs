{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module BowBot.Hypixel.TimeStats where

import BowBot.Hypixel.Stats
import BowBot.Hypixel.Leaderboard
import BowBot.Minecraft.Basic
import BowBot.DB.Basic
import BowBot.Utils
import BowBot.Settings.Basic
import BowBot.Discord.Utils
import qualified Data.Text as T

data StatsTimeRange = DailyStats | WeeklyStats | MonthlyStats deriving (Show, Eq)

instance Param StatsTimeRange
instance Result StatsTimeRange

deriving via (SimpleValue StatsTimeRange) instance ToMysql StatsTimeRange
deriving via (SimpleValue StatsTimeRange) instance FromMysql StatsTimeRange

instance ToField StatsTimeRange where
  toField DailyStats = "daily"
  toField WeeklyStats = "weekly"
  toField MonthlyStats = "monthly"

instance FromField StatsTimeRange where
  fromField = (textSqlTypes, \case
    "ban" -> Right DailyStats
    "default" -> Right WeeklyStats
    "mod" -> Right MonthlyStats
    _ -> Left "Wrong stats time range")

data HypixelBowTimeStats = HypixelBowTimeStats
  { bowTimeWins :: Integer,
    bowTimeLosses :: Integer,
    bowTimeTimestamp :: UTCTime
  } deriving stock (Show, Eq, Generic)
    deriving (ToMysql, FromMysql) via (Generically HypixelBowTimeStats)

$(pure [])

statsTimeRangeName :: StatsTimeRange -> Text
statsTimeRangeName DailyStats = "Day"
statsTimeRangeName WeeklyStats = "Week"
statsTimeRangeName MonthlyStats = "Month"

hypixelBowStatsToTimeStats :: HypixelBowStats -> HypixelBowTimeStats
hypixelBowStatsToTimeStats HypixelBowStats {..} = HypixelBowTimeStats { bowTimeWins = bowWins, bowTimeLosses = bowLosses, bowTimeTimestamp = bowStatsTimestamp }

hypixelBowLeaderboardToTimeStats :: HypixelBowLeaderboardEntry -> HypixelBowTimeStats
hypixelBowLeaderboardToTimeStats HypixelBowLeaderboardEntry {..} = HypixelBowTimeStats { bowTimeWins = bowLbWins, bowTimeLosses = bowLbLosses, bowTimeTimestamp = bowLbTimestamp }

showHypixelBowTimeStats :: StatsTimeRange -> Settings -> HypixelBowStats -> HypixelBowTimeStats -> Text
showHypixelBowTimeStats timeRange Settings {..} HypixelBowStats {..} HypixelBowTimeStats {..} = T.unlines $ catMaybes
  [ Just $ ("- *Since:* " <>) . discordFormatTimestampFull $ bowTimeTimestamp
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

showMaybeHypixelBowTimeStats :: StatsTimeRange -> Settings -> HypixelBowStats -> Maybe HypixelBowTimeStats -> Text
showMaybeHypixelBowTimeStats DailyStats _ _ Nothing = "- **Daily data isn't avaliable yet for this player! Wait until tomorrow!**"
showMaybeHypixelBowTimeStats WeeklyStats _ _ Nothing = "- **Weekly data isn't avaliable yet for this player! Wait until next week!**"
showMaybeHypixelBowTimeStats MonthlyStats _ _ Nothing = "- **Monthly data isn't avaliable yet for this player! Wait until next month!**"
showMaybeHypixelBowTimeStats tm s t (Just v) = showHypixelBowTimeStats tm s t v

updateHypixelBowTimeStats :: (MonadIOReader m r, Has Connection r) => StatsTimeRange -> m ()
updateHypixelBowTimeStats time = void $ executeLog [mysql|INSERT INTO `hypixel_bow_timed_stats`(`minecraft_uuid`,`time`,HypixelBowTimeStats) SELECT `minecraft_uuid`,time, (`wins`,`losses`,`last_update`) FROM `hypixel_bow_stats`|]

getHypixelBowTimeStatsByUUID :: (MonadIOReader m r, Has Connection r) => StatsTimeRange -> UUID -> m (Maybe HypixelBowTimeStats)
getHypixelBowTimeStatsByUUID time uuid = queryOnlyLog [mysql|SELECT HypixelBowTimeStats FROM `hypixel_bow_timed_stats` WHERE `time` = time AND `minecraft_uuid` = uuid|]

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