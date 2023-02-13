{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Database.MySQL.Simple.QueryParams (QueryParams(..))
import Database.MySQL.Simple.QueryResults (QueryResults(..))

data StatsTimeRange = DailyStats | WeeklyStats | MonthlyStats deriving (Show, Eq)

data HypixelBowTimeStats = HypixelBowTimeStats
  { bowTimeWins :: Integer,
    bowTimeLosses :: Integer,
    bowTimeTimestamp :: Maybe UTCTime
  } deriving (Show, Eq)

instance QueryParams HypixelBowTimeStats where
  renderParams HypixelBowTimeStats {..} = renderParams (bowTimeWins, bowTimeLosses, bowTimeTimestamp)
instance QueryResults HypixelBowTimeStats where
  convertResults fields strings = let
    (bowTimeWins, bowTimeLosses, nullZeroTime -> bowTimeTimestamp) = convertResults fields strings
      in HypixelBowTimeStats {..}
instance QueryResultsSize HypixelBowTimeStats where
  queryResultsSize _ = 3

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
  [ ("*Since:* " <>) . discordFormatTimestampFull <$> bowTimeTimestamp
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
    winLossRatio = showWLR (bowWins - bowTimeWins) (bowLosses - bowTimeLosses)

showMaybeHypixelBowTimeStats :: StatsTimeRange -> Settings -> HypixelBowStats -> Maybe HypixelBowTimeStats -> Text
showMaybeHypixelBowTimeStats DailyStats _ _ Nothing = "**Daily data isn't avaliable yet for this player! Wait until tomorrow!**"
showMaybeHypixelBowTimeStats WeeklyStats _ _ Nothing = "**Weekly data isn't avaliable yet for this player! Wait until next week!**"
showMaybeHypixelBowTimeStats MonthlyStats _ _ Nothing = "**Monthly data isn't avaliable yet for this player! Wait until next month!**"
showMaybeHypixelBowTimeStats tm s t (Just v) = showHypixelBowTimeStats tm s t v

updateHypixelBowTimeStats :: (MonadIOReader m r, Has Connection r) => StatsTimeRange -> m ()
updateHypixelBowTimeStats time = void $ executeLog (replaceQuery "TIME" (statsTimeRangeName time) "UPDATE `stats` SET `lastTIMEWins`=`bowWins`, `lastTIMELosses`=`bowLosses`, `lastTIMEUpdate`=`lastUpdate`") ()

getHypixelBowTimeStatsByUUID :: (MonadIOReader m r, Has Connection r) => StatsTimeRange -> UUID -> m (Maybe HypixelBowTimeStats)
getHypixelBowTimeStatsByUUID time uuid = only <$> queryLog (replaceQuery "TIME" (statsTimeRangeName time) "SELECT `lastTIMEWins`, `lastTIMELosses`, `lastTIMEUpdate` FROM `stats` WHERE `minecraft` = ? AND `lastTIMEWins` >= 0 AND `lastTIMELosses` >= 0") (Only uuid)