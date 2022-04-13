{-# LANGUAGE DerivingVia #-}

module BowBot.Network.Monad where

import BowBot.Utils
import Network.HTTP.Client.Conduit (Manager)
import BowBot.Network.Class
import BowBot.DB.Class (MonadDB)
import Control.Monad.Cont (MonadTrans)
import Control.Monad.Reader (ReaderT(..))
import BowBot.Discord.Class (MonadDiscord)

newtype NetworkT m a = NetworkT { runNetworkT :: Manager -> m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadHoistIO, MonadDB, MonadDiscord) via (ReaderT Manager m)
  deriving (MonadTrans) via (ReaderT Manager)

instance MonadIO m => MonadNetwork (NetworkT m) where
  hManager = NetworkT return