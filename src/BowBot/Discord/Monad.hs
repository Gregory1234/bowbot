{-# LANGUAGE DerivingVia #-}

module BowBot.Discord.Monad where

import Discord (DiscordHandle)
import BowBot.Discord.Class
import BowBot.Utils
import Control.Monad.Reader (ReaderT(..))

newtype DiscordHandlerT m a = DiscordHandlerT { runDiscordHandlerT :: DiscordHandle -> m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadHoistIO) via (ReaderT DiscordHandle m)

instance MonadIO m => MonadDiscord (DiscordHandlerT m) where
  liftDiscord h = DiscordHandlerT $ liftIO . runReaderT h