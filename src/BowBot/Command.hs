module BowBot.Command(
  module BowBot.Command, module BowBot.CommandHandler, module BowBot.CommandMonads, module BowBot.API,
  module Discord, module Discord.Types, module BowBot.BotData, module BowBot.Utils
) where

import Discord
import BowBot.Utils
import Discord.Types hiding (accountId)
import BowBot.BotData
import BowBot.CommandMonads
import BowBot.CommandHandler
import BowBot.API

data Command = Command { commandName :: String, commandPerms :: PermissionLevel, commandTimeout :: Int, commandHandler :: CommandHandler () }

registerMessage :: String
registerMessage = "*You aren't on the list! To register, type `?register yourign`.*"

discordNotFoundMessage :: String
discordNotFoundMessage = "*The discord user is not registered!*"

wrongSyntaxMessage :: String
wrongSyntaxMessage = "*Wrong command syntax!*"

playerNotFoundMessage :: String
playerNotFoundMessage = "*The player doesn't exist!*"

somethingWrongMessage :: String
somethingWrongMessage = "*Something went wrong!*"