module BowBot.Account.InfoCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import Discord.Types
import BowBot.Discord.Account
import BowBot.Account.Basic
import BowBot.BotData.Cached (getCacheMap)
import qualified Data.HashMap.Strict as HM
import BowBot.Minecraft.Basic (uuidString)
import qualified Data.Text as T
import BowBot.Utils

infoCommand :: Command
infoCommand = Command CommandInfo
  { commandName = "i"
  , commandHelpEntries = [HelpEntry { helpUsage = "i [name]", helpDescription = "show info about a player", helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument $ \str -> do
    undefined