module BowBot.Network.Class where

import BowBot.Utils
import Network.HTTP.Conduit (Manager)
import Data.ByteString.Lazy.Char8 (ByteString)
import BowBot.Network.Basic
import Control.Monad.Reader
import Control.Monad.Except

class MonadIO m => MonadNetwork m where
  hManager :: m Manager
  
hSendRequestTo :: MonadNetwork m => String -> String -> m ByteString
hSendRequestTo u c = do
  man <- hManager
  liftIO $ sendRequestTo man u c

instance MonadNetwork m => MonadNetwork (ReaderT r m) where
  hManager = lift hManager

instance MonadNetwork m => MonadNetwork (ExceptT e m) where
  hManager = lift hManager