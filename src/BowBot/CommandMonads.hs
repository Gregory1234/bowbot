{-# LANGUAGE DerivingVia #-}

module BowBot.CommandMonads where

import Network.HTTP.Conduit hiding (path)
import BowBot.Utils
import Control.Monad.Reader (ReaderT(..), MonadTrans(..))
import Database.MySQL.Simple (Connection)
import BowBot.BotData.Core (BotData)
import Discord (DiscordHandler)

class MonadIO m => APIMonad m where
  hManager :: m Manager

newtype ManagerT m a = ManagerT { runManagerT :: Manager -> m a }
  deriving (Functor, Applicative, Monad, MonadIO, DBMonad, BotDataMonad) via (ReaderT Manager m)
  deriving (MonadTrans) via (ReaderT Manager)

instance MonadIO m => APIMonad (ManagerT m) where
  hManager = ManagerT return

instance APIMonad m => APIMonad (ReaderT r m) where
  hManager = lift hManager

class MonadIO m => DBMonad m where
  hConnection :: m Connection

newtype ConnectionT m a = ConnectionT { runConnectionT :: Connection -> m a }
  deriving (Functor, Applicative, Monad, MonadIO, APIMonad, BotDataMonad) via (ReaderT Connection m)
  deriving (MonadTrans) via (ReaderT Connection)

instance MonadIO m => DBMonad (ConnectionT m) where
  hConnection = ConnectionT return

instance DBMonad m => DBMonad (ReaderT r m) where
  hConnection = lift hConnection

class Monad m => BotDataMonad m where
  hData :: m BotData

newtype BotDataT m a = BotDataT { runBotDataT :: BotData -> m a }
  deriving (Functor, Applicative, Monad, MonadIO, APIMonad, DBMonad) via (ReaderT BotData m)
  deriving (MonadTrans) via (ReaderT BotData)

instance Monad m => BotDataMonad (BotDataT m) where
  hData = BotDataT return

instance BotDataMonad m => BotDataMonad (ReaderT r m) where
  hData = lift hData

class MonadIO m => DiscordMonad m where
  hDiscord :: DiscordHandler a -> m a

newtype DiscordHandler' a = DiscordHandler' { runDiscordHandler' :: DiscordHandler a }
  deriving (Functor, Applicative, Monad, MonadIO) via DiscordHandler

instance DiscordMonad DiscordHandler' where
  hDiscord = DiscordHandler'

instance DiscordMonad m => DiscordMonad (ReaderT r m) where
  hDiscord = ReaderT . const . hDiscord