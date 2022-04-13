module BowBot.HoistIO where

import Control.Monad.Reader (ReaderT(..))
import Control.Monad.IO.Class (MonadIO)



class MonadIO m => MonadHoistIO m where
  hoistIO :: (IO a -> IO b) -> m a -> m b
  hoistIOWithArg :: ((c -> IO a) -> IO b) -> (c -> m a) -> m b

instance MonadHoistIO IO where
  hoistIO = id
  hoistIOWithArg = id

instance MonadHoistIO m => MonadHoistIO (ReaderT r m) where
  hoistIO f (ReaderT a) = ReaderT $ hoistIO f . a
  hoistIOWithArg f g = ReaderT $ \r -> hoistIOWithArg f (\a -> runReaderT (g a) r)