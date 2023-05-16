module BowBot.Account.RegisterCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Account.Basic
import BowBot.Discord.Utils
import BowBot.BotData.Cached (storeInCache, getFromCache)
import BowBot.Hypixel.Basic (HypixelApi(..))
import BowBot.Hypixel.Leaderboard
import BowBot.Counter.Basic (tryIncreaseCounter)
import BowBot.Account.Register
import BowBot.Discord.Roles
import BowBot.BotData.Info (askInfo, discordGuildIdInfo)
import BowBot.Hypixel.Stats (requestHypixelBowStats)
import Control.Monad.Except
import BowBot.Discord.Account
import BowBot.Hypixel.LeaderboardStatus

registerCommand :: Command
registerCommand = Command CommandInfo
  { commandName = "register"
  , commandHelpEntries = [HelpEntry { helpUsage = "register [name]", helpDescription = "register your Minecraft name in Bow Bot", helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 30
  } $ oneArgument $ \name ->
    undefined

addCommand :: Command
addCommand = Command CommandInfo
  { commandName = "add"
  , commandHelpEntries = [HelpEntry { helpUsage = "add [discord] [name]", helpDescription = "register someone in Bow Bot", helpGroup = "normal" }]
  , commandPerms = ModLevel
  , commandTimeout = 30
  } $ twoArguments $ \did name ->
    undefined

addaltCommand :: Command
addaltCommand = Command CommandInfo
  { commandName = "addalt"
  , commandHelpEntries = [HelpEntry { helpUsage = "addalt [discord] [name]", helpDescription = "register someone's alt account in Bow Bot", helpGroup = "normal" }]
  , commandPerms = ModLevel
  , commandTimeout = 30
  } $ twoArguments $ \did name -> do
    undefined