{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module BowBot.Discord.Monad where

import Discord (DiscordHandle)
import BowBot.Discord.Class
import BowBot.Utils
import Control.Monad.Reader (ReaderT(..), MonadReader(..), MonadFix)
import BowBot.Network.Class (MonadNetwork)
import BowBot.DB.Class (MonadDB)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Writer.Class (MonadWriter)
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)

newtype DiscordHandlerT m a = DiscordHandlerT { runDiscordHandlerT :: DiscordHandle -> m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadHoistIO, MonadNetwork, MonadDB, MonadError e,
            MonadState s, MonadWriter w, MonadFail, MonadFix, Alternative, MonadPlus) via (ReaderT DiscordHandle m)

instance MonadIO m => MonadDiscord (DiscordHandlerT m) where
  liftDiscord h = DiscordHandlerT $ liftIO . runReaderT h

instance MonadReader r m => MonadReader r (DiscordHandlerT m) where
  ask = DiscordHandlerT $ const ask
  local f (DiscordHandlerT g) = DiscordHandlerT $ local f . g