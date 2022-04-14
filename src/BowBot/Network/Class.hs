module BowBot.Network.Class where

import BowBot.Utils
import Network.HTTP.Conduit (Manager)
import Data.ByteString.Lazy.Char8 (ByteString)
import BowBot.Network.Basic
import Control.Monad.Reader (ReaderT(..), lift)
import BowBot.DB.Class

class MonadIO m => MonadNetwork m where
  hManager :: m Manager

instance MonadNetwork m => MonadNetwork (ReaderT r m) where
  hManager = lift hManager
  
hSendRequestTo :: (MonadNetwork m, MonadDB m) => String -> String -> m ByteString
hSendRequestTo u c = do
  man <- hManager
  conn <- hConnection
  liftIO $ sendRequestTo conn man u c