{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module BowBot.Discord.Basic(
  module BowBot.Discord.Basic, module Discord.Types, DiscordHandler, MonadIO(..), MonadReader(..), asks, Has(..)
) where

import BowBot.Discord.DiscordNFData ()
import Discord
import Discord.Types
import qualified Discord.Internal.Rest as R
import BowBot.Utils
import Control.DeepSeq
import Control.Exception.Base (evaluate)
import BowBot.DB.Basic (logError)

liftDiscord :: (MonadIOReader m r, Has DiscordHandle r) => DiscordHandler a -> m a
liftDiscord h = asks getter >>= liftIO . runReaderT h

call :: (FromJSON a, R.Request (rq a), NFData (rq a), MonadIOReader m r, Has DiscordHandle r) => rq a -> m (Either RestCallErrorCode a)
call r = liftDiscord $ liftIO (evaluate (force r)) >>= restCall

call_ :: (FromJSON a, R.Request (rq a), NFData (rq a), MonadIOReader m r, Has DiscordHandle r) => rq a -> m ()
call_ r = (call r >>=) $ \case
  Left e -> logError $ show e
  Right _ -> pure ()