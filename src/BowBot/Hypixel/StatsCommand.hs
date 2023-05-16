module BowBot.Hypixel.StatsCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Hypixel.Stats
import BowBot.Settings.Basic
import BowBot.Utils
import BowBot.Hypixel.Basic (HypixelApi(..))
import BowBot.Counter.Basic
import Control.Monad.Error.Class (throwError)
import Discord.Types
import BowBot.Hypixel.Leaderboard
import BowBot.Hypixel.Announce
import BowBot.BotData.Cached (storeInCache)
import BowBot.Discord.Roles
import BowBot.Command.Tips
import BowBot.Hypixel.LeaderboardStatus

hypixelStatsCommand :: SettingsSource -> Text -> Text -> Command
hypixelStatsCommand src name desc = Command CommandInfo
  { commandName = name
  , commandHelpEntries = [HelpEntry { helpUsage = name <> " [name]", helpDescription = desc, helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument $ \str -> do
    undefined