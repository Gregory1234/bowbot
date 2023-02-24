module BowBot.Hypixel.BanCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Discord.Utils
import BowBot.Minecraft.Arg
import BowBot.BotData.Cached
import BowBot.Hypixel.Leaderboard
import qualified Data.HashMap.Strict as HM
import BowBot.DB.Basic (withDB, executeLog')
import BowBot.Minecraft.Basic (uuidString)
import Database.MySQL.Simple.Types
import BowBot.Hypixel.LeaderboardStatus
  
hypixelBanCommand :: Command
hypixelBanCommand = Command CommandInfo
  { commandName = "sban" -- TODO: should it ban all accounts if provided a discord id?
  , commandHelpEntries = [HelpEntry { helpUsage = "sban [name]", helpDescription = "ban a player from the leaderboard", helpGroup = "normal" }]
  , commandPerms = ModLevel
  , commandTimeout = 15
  } $ oneArgument (getMinecraftAccountByCurrentNameFromCache >=> liftMaybe thePlayerDoesNotExistMessage) $ \mc -> do
    banned <- getHypixelIsBannedByUUID (mcUUID mc)
    if banned == Banned
      then respond "*The player is already banned!*"
      else do
        a <- setHypixelIsBannedByUUID (mcUUID mc) Banned
        b <- removeHypixelBowLeaderboardEntryByUUID (mcUUID mc)
        respond $ if a && b then "*Success, player got banned!*" else somethingWentWrongMessage