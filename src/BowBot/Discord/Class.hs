module BowBot.Discord.Class where

import Discord
import qualified Discord.Internal.Rest as R
import Control.Monad.Reader (ReaderT(..), lift)
import Control.DeepSeq (NFData, force)
import BowBot.Utils
import Control.Exception.Base (evaluate)


class Monad m => MonadDiscord m where
  liftDiscord :: DiscordHandler a -> m a

instance MonadDiscord m => MonadDiscord (ReaderT r m) where
  liftDiscord = lift . liftDiscord

call :: (FromJSON a, R.Request (r a), NFData (r a), MonadDiscord m) => r a -> m (Either RestCallErrorCode a)
call r = liftDiscord $ liftIO (evaluate (force r)) >>= restCall

call_ :: (FromJSON a, R.Request (r a), NFData (r a), MonadDiscord m) => r a -> m ()
call_ r = void $ call r