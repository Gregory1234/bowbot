module BowBot.Hypixel.TimeStatsCommand where

import BowBot.Command
import BowBot.Settings.Basic
import BowBot.Minecraft.Account
import BowBot.Hypixel.Basic
import BowBot.Hypixel.Stats
import BowBot.Hypixel.TimeStats
import BowBot.BotData.Cached
import Control.Monad.Error.Class (throwError)
import BowBot.Counter.Basic
import BowBot.Discord.Roles
import BowBot.Hypixel.Leaderboard
import BowBot.Hypixel.Announce
import BowBot.Discord.Utils
import BowBot.Command.Tips
import BowBot.Hypixel.LeaderboardStatus


hypixelTimeStatsCommand :: SettingsSource -> Text -> Text -> Command
hypixelTimeStatsCommand src name desc = Command CommandInfo
  { commandName = name
  , commandHelpEntries = [HelpEntry { helpUsage = name <> " [name]", helpDescription = desc, helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument $ \str -> do
    undefined