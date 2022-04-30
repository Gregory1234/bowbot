module BowBot.Discord.Class where

import Discord
import qualified Discord.Internal.Rest as R
import Control.Monad.Reader
import Control.DeepSeq (NFData, force)
import Control.Exception.Base (evaluate)
import Control.Monad.Except


class MonadIO m => MonadDiscord m where
  liftDiscord :: DiscordHandler a -> m a

call :: (FromJSON a, R.Request (r a), NFData (r a), MonadDiscord m) => r a -> m (Either RestCallErrorCode a)
call r = liftDiscord $ liftIO (evaluate (force r)) >>= restCall

call_ :: (FromJSON a, R.Request (r a), NFData (r a), MonadDiscord m) => r a -> m ()
call_ r = void $ call r

instance MonadDiscord m => MonadDiscord (ReaderT r m) where
  liftDiscord = lift . liftDiscord

instance MonadDiscord m => MonadDiscord (ExceptT e m) where
  liftDiscord = lift . liftDiscord