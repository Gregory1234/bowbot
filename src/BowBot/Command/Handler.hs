{-# LANGUAGE TypeOperators #-}

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
import Data.Has

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

type CommandHandlerContext = CommandEnvironment :*: Manager :*: DiscordHandle :*: BotData

type CommandHandler = ReaderT CommandHandlerContext IO

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
