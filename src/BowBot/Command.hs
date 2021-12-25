module BowBot.Command(
  module BowBot.Command, module BowBot.CommandHandler, module Discord, module Discord.Types, module BowBot.BotData, module BowBot.Utils
) where

import Discord
import BowBot.Utils
import qualified Discord.Requests as R
import qualified Discord.Internal.Rest as R
import Discord.Types hiding (accountId)
import BowBot.BotData
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Control.Exception.Base (evaluate)
import Control.DeepSeq (force, NFData(..))
import BowBot.DiscordNFData()
import BowBot.CommandHandler

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