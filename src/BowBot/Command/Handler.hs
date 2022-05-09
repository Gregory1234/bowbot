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
import BowBot.Network.Class (MonadNetwork)
import BowBot.Discord.Class
import Discord.Types
import Control.Monad.Reader (ReaderT(..))
import BowBot.BotMonad
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

newtype CommandHandler a = CommandHandler { runCommandHandler :: CommandEnvironment -> BotData -> Manager -> DiscordHandler a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadNetwork, MonadDiscord, MonadHoistIO) via (ReaderT CommandEnvironment Bot)

deriving via (ReaderT CommandEnvironment Bot) instance (MonadCache c Bot, MonadIO CommandHandler) => MonadCache c CommandHandler

deriving via (ReaderT CommandEnvironment Bot) instance (Counted c, MonadCounter c Bot, MonadIO CommandHandler) => MonadCounter c CommandHandler

deriving via (ReaderT CommandEnvironment Bot) instance (MonadCacheSingle c Bot, MonadIO CommandHandler) => MonadCacheSingle c CommandHandler

hEnv :: (CommandEnvironment -> v) -> CommandHandler v
hEnv f = CommandHandler $ \e _ _ -> return $ f e 

hRespond :: String -> CommandHandler ()
hRespond m = do
  res <- hEnv envRespond
  res m

hRespondFile :: String -> String -> CommandHandler ()
hRespondFile n m = do
  res <- hEnv envRespondFile
  res n m
