module BowBot.Hypixel.Stats.Command where

import BowBot.Command
import BowBot.Hypixel.Stats
import BowBot.Settings.Basic
import BowBot.Utils
import BowBot.Hypixel.Leaderboard
import BowBot.Hypixel.Announce
import BowBot.Discord.Roles
import BowBot.Hypixel.Ban.Status
import BowBot.Hypixel.CommandUtils
import BowBot.Hypixel.TimeStats
import BowBot.Stats.Command

hypixelStatsCommand :: SettingsSource -> Text -> Text -> Command
hypixelStatsCommand = statsCommandTemplate StatsCommandSettings
  { requestStats = \b -> liftMaybe (if b then youNeverJoinedHypixelMessage else thePlayerNeverJoinedHypixelMessage) <=< hypixelSafeRequestStats
  , shouldAddAccount = (>= 50) . bowWins
  , updateStats = \uuid stats -> do
      void $ setHypixelBowLeaderboardEntryByUUID uuid (hypixelBowStatsToLeaderboards stats)
      applyRolesDivisionTitleByUUID uuid
      announceMilestones
  , showStats = showHypixelBowStats
  , getIsBanned = getHypixelIsBannedByUUID
  }

hypixelTimeStatsCommand :: SettingsSource -> Text -> Text -> Command
hypixelTimeStatsCommand = statsCommandTemplate StatsCommandSettings
  { requestStats = \b uuid -> do
    currentHypixelBowStats <- liftMaybe (if b then youNeverJoinedHypixelMessage else thePlayerNeverJoinedHypixelMessage) =<< hypixelSafeRequestStats uuid
    dailyHypixelBowStats <- getHypixelBowTimeStatsByUUID DailyStats uuid
    weeklyHypixelBowStats <- getHypixelBowTimeStatsByUUID WeeklyStats uuid
    monthlyHypixelBowStats <- getHypixelBowTimeStatsByUUID MonthlyStats uuid
    return FullHypixelBowTimeStats {..}
  , shouldAddAccount = (>= 50) . bowWins . currentHypixelBowStats
  , updateStats = \uuid stats -> do
      void $ setHypixelBowLeaderboardEntryByUUID uuid (hypixelBowStatsToLeaderboards (currentHypixelBowStats stats))
      applyRolesDivisionTitleByUUID uuid
      announceMilestones
  , showStats = showFullHypixelBowTimeStats
  , getIsBanned = getHypixelIsBannedByUUID
  }