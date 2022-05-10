{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BowBot.Command.Handler where

import BowBot.Utils
import Discord
import qualified Discord.Requests as R
import BowBot.Network.Basic
import BowBot.Discord.Basic
import Data.Text.Encoding (encodeUtf8)
import BowBot.Discord.DiscordNFData ()
import BowBot.BotData.Basic
import Data.Has
import BowBot.BotData.HasData

newtype CommandArgs = CommandMessageArgs [String]

data CommandEnvironment = CommandEnvironment
  { envSender :: User
  , envSenderMember :: Maybe GuildMember
  , envChannel :: ChannelId
  , envGuild :: GuildId
  , envRespond :: forall m r. (MonadIO m, MonadReader r m, Has DiscordHandle r) => String -> m ()
  , envRespondFile :: forall m r. (MonadIO m, MonadReader r m, Has DiscordHandle r) => String -> String -> m ()
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

data CommandHandlerContext = CommandHandlerContext
  { cctxEnv :: CommandEnvironment
  , cctxManager :: Manager
  , cctxDiscord :: DiscordHandle
  , cctxData :: BotData
  }

instance Has CommandEnvironment CommandHandlerContext where
  getter = cctxEnv
  modifier f x = x { cctxEnv = f $ cctxEnv x }
instance Has Manager CommandHandlerContext where
  getter = cctxManager
  modifier f x = x { cctxManager = f $ cctxManager x }
instance Has DiscordHandle CommandHandlerContext where
  getter = cctxDiscord
  modifier f x = x { cctxDiscord = f $ cctxDiscord x }
instance Has BotData CommandHandlerContext where
  getter = cctxData
  modifier f x = x { cctxData = f $ cctxData x }
instance HasBotData BotData CommandHandlerContext

type CommandHandler = ReaderT CommandHandlerContext IO

envs :: (MonadReader r m, Has CommandEnvironment r) => (CommandEnvironment -> v) -> m v
envs f = asks $ f . getter

respond :: (MonadIO m, MonadReader r m, Has CommandEnvironment r, Has DiscordHandle r) => String -> m ()
respond m = do
  res <- envs envRespond
  res m

respondFile :: (MonadIO m, MonadReader r m, Has CommandEnvironment r, Has DiscordHandle r) => String -> String -> m ()
respondFile n m = do
  res <- envs envRespondFile
  res n m
