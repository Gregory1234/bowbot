module BowBot.Hypixel.BanCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Discord.Utils
import BowBot.Hypixel.Leaderboard
import BowBot.Hypixel.LeaderboardStatus
  
hypixelBanCommand :: Command
hypixelBanCommand = Command CommandInfo
  { commandName = "sban" -- TODO: should it ban all accounts if provided a discord id?
  , commandHelpEntries = [HelpEntry { helpUsage = "sban [name]", helpDescription = "ban a player from the leaderboard", helpGroup = "normal" }]
  , commandPerms = ModLevel
  , commandTimeout = 15
  } $ oneArgument $ \str -> do
    undefined