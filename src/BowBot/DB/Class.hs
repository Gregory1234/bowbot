module BowBot.DB.Class where

import BowBot.Utils
import BowBot.DB.Basic
import Database.MySQL.Simple (Connection, Query, insertID, withTransaction)
import Database.MySQL.Simple.QueryParams (QueryParams)
import Database.MySQL.Simple.QueryResults (QueryResults)
import Data.Int (Int64)
import Data.Word (Word64)
import Control.Monad.Reader (ReaderT(..), lift)
  
class MonadIO m => MonadDB m where
  hConnection :: m Connection

instance MonadDB m => MonadDB (ReaderT r m) where
  hConnection = lift hConnection

hLogInfoDB :: MonadDB m => String -> m ()
hLogInfoDB str = hConnection >>= flip logInfoDB str

hLogErrorDB :: MonadDB m => String -> m ()
hLogErrorDB str = hConnection >>= flip logErrorDB str

hQueryLog :: (QueryParams q, QueryResults r, MonadDB m) => Query -> q -> m [r]
hQueryLog q d = do
  conn <- hConnection
  queryLog conn q d

hExecuteLog :: (QueryParams q, MonadDB m) => Query -> q -> m Int64
hExecuteLog q d = do
  conn <- hConnection
  executeLog conn q d

hExecuteManyLog :: (QueryParams q, MonadDB m) => Query -> [q] -> m Int64
hExecuteManyLog q d = do
  conn <- hConnection
  executeManyLog conn q d

hInsertID :: MonadDB m => m Word64
hInsertID = hConnection >>= liftIO . insertID

hTransaction :: (MonadDB m, MonadHoistIO m) => m a -> m a
hTransaction act = do
  conn <- hConnection
  hoistIO (withTransaction conn) act