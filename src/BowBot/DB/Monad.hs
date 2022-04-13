{-# LANGUAGE DerivingVia #-}

module BowBot.DB.Monad where

import BowBot.DB.Class
import BowBot.Utils
import Control.Monad.Cont (MonadTrans)
import Control.Monad.Reader (ReaderT(..))
import Database.MySQL.Simple (Connection)
import BowBot.Network.Class (MonadNetwork)
import BowBot.Discord.Class (MonadDiscord)


newtype DatabaseT m a = DatabaseT { runDatabaseT :: Connection -> m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadHoistIO, MonadNetwork, MonadDiscord) via (ReaderT Connection m)
  deriving (MonadTrans) via (ReaderT Connection)

instance MonadIO m => MonadDB (DatabaseT m) where
  hConnection = DatabaseT return