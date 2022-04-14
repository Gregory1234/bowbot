{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module BowBot.Network.Monad where

import BowBot.Utils
import Network.HTTP.Client.Conduit (Manager)
import BowBot.Network.Class
import BowBot.DB.Class (MonadDB)
import Control.Monad.Cont (MonadTrans)
import Control.Monad.Reader (ReaderT(..), MonadReader(..), MonadFix)
import BowBot.Discord.Class (MonadDiscord)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Writer.Class (MonadWriter)
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)

newtype NetworkT m a = NetworkT { runNetworkT :: Manager -> m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadHoistIO, MonadDB, MonadDiscord, MonadError e,
            MonadState s, MonadWriter w, MonadFail, MonadFix, Alternative, MonadPlus) via (ReaderT Manager m)
  deriving (MonadTrans) via (ReaderT Manager)

instance MonadIO m => MonadNetwork (NetworkT m) where
  hManager = NetworkT return

instance MonadReader r m => MonadReader r (NetworkT m) where
  ask = NetworkT $ const ask
  local f (NetworkT g) = NetworkT $ local f . g