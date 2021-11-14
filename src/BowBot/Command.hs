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
import Control.DeepSeq (force)

data Command = Command { commandName :: String, commandPerms :: PermissionLevel, commandTimeout :: Int, commandHandler :: Message -> Manager -> BotData -> DiscordHandler () }

call :: (FromJSON a, R.Request (r a)) => r a -> DiscordHandler ()
call r = do
  _ <- liftIO $ evaluate r
  void $ restCall r

respond :: Message -> String -> DiscordHandler ()
respond m s = do
  _ <- liftIO $ evaluate $ force s
  call $ R.CreateMessage (messageChannel m) $ pack s

respondFile :: Message -> T.Text -> String -> DiscordHandler ()
respondFile m n s = do
  _ <- liftIO $ evaluate $ force n
  _ <- liftIO $ evaluate $ force s
  call $ R.CreateMessageUploadFile (messageChannel m) n $ encodeUtf8 $ pack s

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