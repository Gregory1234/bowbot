module BowBot.Command(
  module BowBot.Command, module Discord, module Discord.Types, module BowBot.BotData, module BowBot.Utils
) where

import Discord
import BowBot.Utils
import qualified Discord.Requests as R
import qualified Discord.Internal.Rest as R
import Discord.Types hiding (accountId)
import BowBot.BotData
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Conduit (Manager)
import Control.Exception.Base (evaluate)
import Control.DeepSeq (force, NFData(..))
import BowBot.DiscordNFData()

data Command = Command { commandName :: String, commandPerms :: PermissionLevel, commandTimeout :: Int, commandHandler :: Message -> Manager -> BotData -> DiscordHandler () }

call :: (FromJSON a, R.Request (r a), NFData (r a)) => r a -> DiscordHandler (Either RestCallErrorCode a)
call r = liftIO (evaluate (force r)) >>= restCall

call_ :: (FromJSON a, R.Request (r a), NFData (r a)) => r a -> DiscordHandler ()
call_ r = void $ call r

respond :: Message -> String -> DiscordHandler ()
respond m s = call_ $ R.CreateMessage (messageChannel m) $ pack s

respondFile :: Message -> T.Text -> String -> DiscordHandler ()
respondFile m n s = call_ $ R.CreateMessageUploadFile (messageChannel m) n $ encodeUtf8 $ pack s

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