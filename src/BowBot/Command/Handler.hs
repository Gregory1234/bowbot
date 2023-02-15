module BowBot.Command.Handler where

import BowBot.Utils
import Discord
import qualified Discord.Requests as R
import BowBot.Network.Basic
import BowBot.Discord.Basic
import Data.Text.Encoding (encodeUtf8)
import BowBot.Discord.Orphans ()
import BowBot.BotData.Basic
import BowBot.BotData.HasData
import BowBot.Counter.Basic
import BowBot.DB.Basic (Connection)
import qualified Data.Text as T

newtype CommandArgs = CommandMessageArgs [Text]

data CommandEnvironment = CommandEnvironment
  { envSender :: !User
  , envSenderMember :: !(Maybe GuildMember)
  , envChannel :: !ChannelId
  , envGuild :: !GuildId
  , envRespond :: forall m r. (MonadIOReader m r, Has DiscordHandle r) => Text -> m ()
  , envRespondFile :: forall m r. (MonadIOReader m r, Has DiscordHandle r) => Text -> Text -> m ()
  , envArgs :: !CommandArgs
  }

commandEnvFromMessage :: Message -> CommandEnvironment
commandEnvFromMessage m = CommandEnvironment
  { envSender = messageAuthor m
  , envSenderMember = messageMember m
  , envChannel = messageChannelId m
  , envGuild = fromMaybe 0 $ messageGuildId m
  , envRespond = call_ . R.CreateMessage (messageChannelId m)
  , envRespondFile = \n s -> call_ $ R.CreateMessageDetailed (messageChannelId m) def { R.messageDetailedFile = Just (n, encodeUtf8 s) }
  , envArgs = CommandMessageArgs $ tail $ T.words $ messageContent m
  }

data CommandHandlerContext = CommandHandlerContext
  { cctxEnv :: !CommandEnvironment
  , cctxManager :: !Manager
  , cctxConnection :: !Connection
  , cctxDiscord :: !DiscordHandle
  , cctxCounter :: !CounterState
  , cctxData :: !BotData
  }

instance Has CommandEnvironment CommandHandlerContext where
  getter = cctxEnv
  modifier f x = x { cctxEnv = f $ cctxEnv x }
instance Has Manager CommandHandlerContext where
  getter = cctxManager
  modifier f x = x { cctxManager = f $ cctxManager x }
instance Has Connection CommandHandlerContext where
  getter = cctxConnection
  modifier f x = x { cctxConnection = f $ cctxConnection x }
instance Has DiscordHandle CommandHandlerContext where
  getter = cctxDiscord
  modifier f x = x { cctxDiscord = f $ cctxDiscord x }
instance Has CounterState CommandHandlerContext where
  getter = cctxCounter
  modifier f x = x { cctxCounter = f $ cctxCounter x }
instance Has BotData CommandHandlerContext where
  getter = cctxData
  modifier f x = x { cctxData = f $ cctxData x }
instance HasBotData BotData CommandHandlerContext

type CommandHandler = ReaderT CommandHandlerContext IO

envs :: (MonadReader r m, Has CommandEnvironment r) => (CommandEnvironment -> v) -> m v
envs f = asks $ f . getter

respond :: (MonadIOReader m r, HasAll [CommandEnvironment, DiscordHandle] r) => Text -> m ()
respond m = do
  res <- envs (\x -> envRespond x)
  res m

respondFile :: (MonadIOReader m r, HasAll [CommandEnvironment, DiscordHandle] r) => Text -> Text -> m ()
respondFile n m = do
  res <- envs (\x -> envRespondFile x)
  res n m
