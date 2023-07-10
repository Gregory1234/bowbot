{-# LANGUAGE TypeFamilies #-}

module BowBot.Hypixel.TimeStats where

import BowBot.Hypixel.Stats
import BowBot.Hypixel.Leaderboard
import BowBot.Minecraft.Basic
import BowBot.DB.Typed
import BowBot.Utils
import BowBot.Settings.Basic
import BowBot.Discord.Utils
import qualified Data.Text as T
import qualified Database.MySQL.Base.Types as T

data StatsTimeRange = DailyStats | WeeklyStats | MonthlyStats
  deriving (Show, Eq)
  deriving (QueryParams, QueryResults) via (SimpleValue StatsTimeRange)

instance Param StatsTimeRange
instance Result StatsTimeRange

instance ToField StatsTimeRange where
  toField DailyStats = "daily"
  toField WeeklyStats = "weekly"
  toField MonthlyStats = "monthly"

instance FromField StatsTimeRange where
  fromField = ([T.Enum, T.String], \case
    "ban" -> Right DailyStats
    "default" -> Right WeeklyStats
    "mod" -> Right MonthlyStats
    _ -> Left "Wrong stats time range")

data HypixelBowTimeStats = HypixelBowTimeStats
  { bowTimeWins :: Integer,
    bowTimeLosses :: Integer,
    bowTimeTimestamp :: Maybe UTCTime
  } deriving (Show, Eq)

instance QueryParams HypixelBowTimeStats where
  renderParams HypixelBowTimeStats {..} = [render bowTimeWins, render bowTimeLosses, render bowTimeTimestamp]
instance QueryResults HypixelBowTimeStats where
  convertResults = HypixelBowTimeStats <$> convert <*> convert <*> fmap nullZeroTime convert
instance DatabaseTable HypixelBowTimeStats where
  type PrimaryKey HypixelBowTimeStats = (UUID, StatsTimeRange)
  databaseTableName _ = "hypixel_bow_timed_stats"
  databaseColumnNames _ = ["wins", "losses", "last_update"]
  databasePrimaryKey _ = ["minecraft_uuid", "time"]

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

showMaybeHypixelBowTimeStats :: StatsTimeRange -> Settings -> HypixelBowStats -> Maybe HypixelBowTimeStats -> Text
showMaybeHypixelBowTimeStats DailyStats _ _ Nothing = "- **Daily data isn't avaliable yet for this player! Wait until tomorrow!**"
showMaybeHypixelBowTimeStats WeeklyStats _ _ Nothing = "- **Weekly data isn't avaliable yet for this player! Wait until next week!**"
showMaybeHypixelBowTimeStats MonthlyStats _ _ Nothing = "- **Monthly data isn't avaliable yet for this player! Wait until next month!**"
showMaybeHypixelBowTimeStats tm s t (Just v) = showHypixelBowTimeStats tm s t v

updateHypixelBowTimeStats :: (MonadIOReader m r, Has Connection r) => StatsTimeRange -> m ()
updateHypixelBowTimeStats time = void $ executeLogT (insertSelectQueryKeyed @HypixelBowTimeStats (TypedQuery "SELECT `minecraft_uuid`,?,`wins`,`losses`,`last_update` FROM `hypixel_bow_stats`")) time

getHypixelBowTimeStatsByUUID :: (MonadIOReader m r, Has Connection r) => StatsTimeRange -> UUID -> m (Maybe HypixelBowTimeStats)
getHypixelBowTimeStatsByUUID time uuid = queryOnlyLogT selectByPrimaryQuery (uuid, time)

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