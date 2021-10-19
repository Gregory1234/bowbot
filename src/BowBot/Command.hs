module BowBot.Command where

import Discord
import qualified Discord.Requests as R
import qualified Discord.Internal.Rest as R
import Discord.Types
import BowBot.BotData
import qualified Data.Text as T
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Control.Monad (void)
import Network.HTTP.Conduit (Manager)

-- TODO: make argument order better
data Command = Command { commandName :: String, commandPerms :: PermissionLevel, commandTimeout :: Int, commandHandler :: Message -> Manager -> BotData -> DiscordHandler () }

call :: (FromJSON a, R.Request (r a)) => r a -> DiscordHandler ()
call = void . restCall

respond :: Message -> String -> DiscordHandler ()
respond m = call . R.CreateMessage (messageChannel m) . pack

respondFile :: Message -> T.Text -> String -> DiscordHandler ()
respondFile m n = call . R.CreateMessageUploadFile (messageChannel m) n . encodeUtf8 . pack

registerMessage :: String
registerMessage = "*You aren't on the list! To register, type `?register yourign`.*"

sendRegisterMessage :: Message -> DiscordHandler ()
sendRegisterMessage m = respond m registerMessage
