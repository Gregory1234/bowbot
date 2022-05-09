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
import BowBot.Network.Basic
import BowBot.Discord.Basic
import Control.Monad.Reader (ReaderT(..))
import Data.Text.Encoding (encodeUtf8)
import BowBot.Discord.DiscordNFData ()
import BowBot.BotData.Basic
import BowBot.BotData.Cached (MonadCache)
import BowBot.BotData.Counter (MonadCounter, Counted)
import BowBot.BotData.CachedSingle (MonadCacheSingle)

newtype CommandArgs = CommandMessageArgs [String]

data CommandEnvironment = CommandEnvironment
  { envSender :: User
  , envSenderMember :: Maybe GuildMember
  , envChannel :: ChannelId
  , envGuild :: GuildId
  , envRespond :: String -> CommandHandler ()
  , envRespondFile :: String -> String -> CommandHandler ()
  , envArgs :: CommandArgs
  }

commandEnvFromMessage :: Message -> CommandEnvironment
commandEnvFromMessage m = CommandEnvironment
  { envSender = messageAuthor m
  , envSenderMember = messageMember m
  , envChannel = messageChannelId m
  , envGuild = fromMaybe 0 $ messageGuildId m
  , envRespond = call_ . R.CreateMessage (messageChannelId m) . pack
  , envRespondFile = \n s -> call_ $ R.CreateMessageDetailed (messageChannelId m) def { R.messageDetailedFile = Just (pack n, encodeUtf8 $ pack s) }
  , envArgs = CommandMessageArgs $ tail $ words $ unpack $ messageContent m
  }

newtype CommandHandler a = CommandHandler { runCommandHandler :: BotData -> (CommandEnvironment, Manager, DiscordHandle) -> IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (CommandEnvironment, Manager, DiscordHandle), MonadHoistIO) via (BotDataT (ReaderT (CommandEnvironment, Manager, DiscordHandle) IO))

deriving via (BotDataT (ReaderT (CommandEnvironment, Manager, DiscordHandle) IO)) instance (MonadCache c (BotDataT (ReaderT (CommandEnvironment, Manager, DiscordHandle) IO)), MonadIO CommandHandler) => MonadCache c CommandHandler

deriving via (BotDataT (ReaderT (CommandEnvironment, Manager, DiscordHandle) IO)) instance (Counted c, MonadCounter c (BotDataT (ReaderT (CommandEnvironment, Manager, DiscordHandle) IO)), MonadIO CommandHandler) => MonadCounter c CommandHandler

deriving via (BotDataT (ReaderT (CommandEnvironment, Manager, DiscordHandle) IO)) instance (MonadCacheSingle c (BotDataT (ReaderT (CommandEnvironment, Manager, DiscordHandle) IO)), MonadIO CommandHandler) => MonadCacheSingle c CommandHandler

hEnv :: (CommandEnvironment -> v) -> CommandHandler v
hEnv f = asks $ f . getter

hRespond :: String -> CommandHandler ()
hRespond m = do
  res <- hEnv envRespond
  res m

hRespondFile :: String -> String -> CommandHandler ()
hRespondFile n m = do
  res <- hEnv envRespondFile
  res n m
