module BowBot.Minecraft.NameCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import Discord.Types
import Control.Monad.Trans (lift)
import qualified Data.Text as T

nameCommand :: Command
nameCommand = Command CommandInfo
  { commandName = "n"
  , commandHelpEntries = [HelpEntry { helpUsage = "n [name]", helpDescription = "show player's Minecraft name history", helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument $ \str -> do
    undefined
