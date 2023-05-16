module BowBot.Minecraft.SelectCommand where

import BowBot.Command
import BowBot.Account.Basic
import BowBot.Minecraft.Account
import BowBot.Discord.Utils
import BowBot.BotData.Cached
import BowBot.DB.Basic
import BowBot.Minecraft.Basic (uuidString)


selectMinecraftCommand :: Command
selectMinecraftCommand = Command CommandInfo
  { commandName = "selectmc"
  , commandHelpEntries = [HelpEntry { helpUsage = "selectmc [name]", helpDescription = "select one of your registered minecraft accounts as a preferred one", helpGroup = "settings" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 30
  } $ oneArgument $ \str -> do
    undefined
