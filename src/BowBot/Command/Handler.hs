{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BowBot.Command.Handler where

import BowBot.Utils
import Discord
import qualified Discord.Requests as R
import Network.HTTP.Conduit (Manager)
import Database.MySQL.Simple (Connection)
import BowBot.Network.Class (MonadNetwork, hManager)
import BowBot.DB.Class (MonadDB, hConnection)
import BowBot.Discord.Class
import Discord.Types
import Control.Monad.Reader (ReaderT(..), ask)
import BowBot.BotMonad
import Data.Text.Encoding (encodeUtf8)
import BowBot.Discord.DiscordNFData ()
import BowBot.Command.Args
import Control.Monad.Except (ExceptT(..), runExceptT)

data CommandEnvironment args = CommandEnvironment
  { envSender :: User
  , envSenderMember :: Maybe GuildMember
  , envChannel :: ChannelId
  , envRespond :: String -> CommandHandler args ()
  , envRespondFile :: String -> String -> CommandHandler args ()
  , envArgs :: BotT (ExceptT String IO) args
  }

commandEnvFromMessage :: CommandArgs desc v => desc -> Message -> CommandEnvironment v
commandEnvFromMessage p m = CommandEnvironment
  { envSender = messageAuthor m
  , envSenderMember = messageMember m
  , envChannel = messageChannelId m
  , envRespond = call_ . R.CreateMessage (messageChannelId m) . pack
  , envRespondFile = \n s -> call_ $ R.CreateMessageDetailed (messageChannelId m) def { R.messageDetailedFile = Just (pack n, encodeUtf8 $ pack s) }
  , envArgs = parseArgsFromStrings p (tail $ words $ unpack $ messageContent m)
  }

newtype CommandHandler args a = CommandHandler { runCommandHandler :: CommandEnvironment args -> Connection -> Manager -> DiscordHandler a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadNetwork, MonadDB, MonadDiscord) via (ReaderT (CommandEnvironment args) Bot)

hEnv :: (CommandEnvironment args -> v) -> CommandHandler args v
hEnv f = CommandHandler $ \e _ _ -> return $ f e 

hRespond :: forall args. String -> CommandHandler args ()
hRespond m = do
  res <- hEnv envRespond
  res m

hRespondFile :: forall args. String -> String -> CommandHandler args ()
hRespondFile n m = do
  res <- hEnv envRespondFile
  res n m

withArgs :: (args -> CommandHandler args ()) -> CommandHandler args ()
withArgs f = do
  maybeArgs' <- hEnv envArgs
  maybeArgsIO <- fmap runExceptT $ runBotT maybeArgs' <$> hConnection <*> hManager <*> liftDiscord ask
  maybeArgs <- liftIO maybeArgsIO
  case maybeArgs of
    Left err -> hRespond err
    Right args -> f args
    