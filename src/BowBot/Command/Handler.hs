{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}

module BowBot.Command.Handler where

import BowBot.Utils
import Discord
import qualified Discord.Requests as R
import Network.HTTP.Conduit (Manager)
import BowBot.Network.Class (MonadNetwork, hManager)
import BowBot.Discord.Class
import Discord.Types
import Control.Monad.Reader (ReaderT(..), ask)
import BowBot.BotMonad
import Data.Text.Encoding (encodeUtf8)
import BowBot.Discord.DiscordNFData ()
import BowBot.Command.Args
import Control.Monad.Except (ExceptT(..), runExceptT)
import BowBot.BotData.Basic
import BowBot.BotData.Cached (MonadCache)

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

newtype CommandHandler args a = CommandHandler { runCommandHandler :: CommandEnvironment args -> BotData -> Manager -> DiscordHandler a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadNetwork, MonadDiscord) via (ReaderT (CommandEnvironment args) Bot)

deriving via (ReaderT (CommandEnvironment args) Bot) instance (MonadCache c Bot, MonadIO (CommandHandler args)) => MonadCache c (CommandHandler args)

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
  maybeArgsIO <- fmap runExceptT $ runBotT maybeArgs' <$> CommandHandler (\_ d _ -> return d) <*> hManager <*> liftDiscord ask
  maybeArgs <- liftIO maybeArgsIO
  case maybeArgs of
    Left err -> hRespond err
    Right args -> f args
    