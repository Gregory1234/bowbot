{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module BowBot.DB.Monad where

import BowBot.DB.Class
import BowBot.Utils
import Control.Monad.Cont (MonadTrans)
import Control.Monad.Reader (ReaderT(..), MonadReader(..), MonadFix)
import Database.MySQL.Simple (Connection)
import BowBot.Network.Class (MonadNetwork)
import BowBot.Discord.Class (MonadDiscord)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Writer.Class (MonadWriter)
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)


newtype DatabaseT m a = DatabaseT { runDatabaseT :: Connection -> m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadHoistIO, MonadNetwork, MonadDiscord, MonadError e, 
            MonadState s, MonadWriter w, MonadFail, MonadFix, Alternative, MonadPlus) via (ReaderT Connection m)
  deriving (MonadTrans) via (ReaderT Connection)

instance MonadIO m => MonadDB (DatabaseT m) where
  hConnection = DatabaseT return

instance MonadReader r m => MonadReader r (DatabaseT m) where
  ask = DatabaseT $ const ask
  local f (DatabaseT g) = DatabaseT $ local f . g