{-# OPTIONS_GHC -fno-warn-orphans #-}

module BowBot.Discord.Basic(
  module BowBot.Discord.Basic, module Discord.Types, DiscordHandler, MonadIO(..), MonadReader(..), asks, Has(..)
) where

import BowBot.Discord.Orphans ()
import Discord
import Discord.Types
import qualified Discord.Internal.Rest as R
import BowBot.Utils
import Control.DeepSeq
import Control.Exception.Base (evaluate)
import BowBot.DB.Basic (logErrorFork)

liftDiscord :: (MonadIOReader m r, Has DiscordHandle r) => DiscordHandler a -> m a
liftDiscord h = asks getter >>= liftIO . runReaderT h

call :: (FromJSON a, R.Request (rq a), NFData (rq a), MonadIOReader m r, Has DiscordHandle r) => rq a -> m (Either RestCallErrorCode a)
call r = liftDiscord $ liftIO (evaluate (force r)) >>= restCall

call_ :: (FromJSON a, R.Request (rq a), NFData (rq a), MonadIOReader m r, Has DiscordHandle r) => rq a -> m ()
call_ r = (call r >>=) $ \case
  Left e -> logErrorFork $ showt e
  Right _ -> pure ()