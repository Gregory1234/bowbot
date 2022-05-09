{-# LANGUAGE FlexibleContexts #-}

module BowBot.Discord.Basic(
  module BowBot.Discord.Basic, module Discord.Types, DiscordHandler, MonadIO(..), MonadReader(..), asks, Has(..)
) where

import BowBot.Discord.DiscordNFData ()
import Discord
import Discord.Types
import qualified Discord.Internal.Rest as R
import BowBot.Utils
import Control.Monad.Reader
import Data.Has
import Control.DeepSeq
import Control.Exception.Base (evaluate)

liftDiscord :: (MonadIO m, MonadReader r m, Has DiscordHandle r) => DiscordHandler a -> m a
liftDiscord h = asks getter >>= liftIO . runReaderT h

call :: (FromJSON a, R.Request (rq a), NFData (rq a), MonadReader r m, Has DiscordHandle r, MonadIO m) => rq a -> m (Either RestCallErrorCode a)
call r = liftDiscord $ liftIO (evaluate (force r)) >>= restCall

call_ :: (FromJSON a, R.Request (rq a), NFData (rq a), MonadReader r m, Has DiscordHandle r, MonadIO m) => rq a -> m ()
call_ r = void $ call r