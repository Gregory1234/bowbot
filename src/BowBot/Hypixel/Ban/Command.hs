module BowBot.Hypixel.Ban.Command where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Discord.Utils
import BowBot.Hypixel.Leaderboard
import BowBot.Hypixel.Ban.Status
import BowBot.Command.Utils
  
hypixelBanCommand :: Command
hypixelBanCommand = Command CommandInfo
  { commandName = "sban" -- TODO: should it ban all accounts if provided a discord id?
  , commandHelpEntries = [HelpEntry { helpUsage = "sban [name]", helpDescription = "ban a player from the leaderboard", helpGroup = "normal" }]
  , commandPerms = ModLevel
  , commandTimeout = 15
  } $ oneArgument $ \name -> do
    mc <- liftMaybe thePlayerDoesNotExistMessage =<< getMinecraftAccountByCurrentName name
    banned <- getHypixelIsBannedByUUID (mcUUID mc)
    if banned == Banned
      then respond "*The player is already banned!*"
      else do
        a <- setHypixelIsBannedByUUID (mcUUID mc) Banned
        b <- removeHypixelBowLeaderboardEntryByUUID (mcUUID mc)
        respond $ if a && b then "*Success, player got banned!*" else somethingWentWrongMessage