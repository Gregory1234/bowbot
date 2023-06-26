module BowBot.DB.Basic(
  module BowBot.DB.Basic, Connection, Only(..), QueryParams(..), QueryResults(..), Param, Result, ToField(..), FromField(..), In(..)
) where

import BowBot.Utils
import Database.MySQL.Simple hiding (withTransaction, commit, rollback, insertID)
import qualified Database.MySQL.Simple as M (withTransaction, commit, rollback, insertID)
import Database.MySQL.Simple.QueryParams (QueryParams(..))
import Database.MySQL.Simple.QueryResults (QueryResults(..))
import Database.MySQL.Simple.Types (Query(..))
import Data.Int (Int64)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Search as BS
import Control.Exception.Base (bracket)
import Control.Concurrent (forkIO)
import Data.Time (getCurrentTime)
import Data.Proxy (Proxy(..))


withDB :: MonadHoistIO m => (Connection -> m a) -> m a -- TODO: report connection errors
withDB f = do
  -- liftIO $ putStrLn "Start connection"
  connectHost <- liftIO $ getEnvOrThrow "DB_HOST"
  connectUser <- liftIO $ getEnvOrThrow "DB_USER"
  connectPassword <- liftIO $ getEnvOrThrow "DB_PASS"
  connectDatabase <- liftIO $ getEnvOrThrow "DB_NAME"
  hoistIOWithArg (bracket (liftIO $ connect $ defaultConnectInfo { connectHost, connectUser, connectPassword, connectDatabase }) close) f

logInfo' :: MonadIO m => Connection -> Text -> m ()
logInfo' conn msg = liftIO $ void $ do
  putStrLn (unpack msg)
  time <- getCurrentTime
  execute conn "INSERT INTO `logs`(`message`, `timestamp`) VALUES (?,?)" (msg, time)

logInfo :: (MonadIOReader m r, Has Connection r) => Text -> m ()
logInfo msg = do
  conn <- asks getter
  logInfo' conn msg

logInfoFork :: MonadIO m => Text -> m ()
logInfoFork msg = void $ liftIO $ do
  putStrLn (unpack msg)
  time <- getCurrentTime
  forkIO $ withDB $ \conn -> void $ execute conn "INSERT INTO `logs`(`message`, `timestamp`) VALUES (?,?)" (msg, time)

logError' :: MonadIO m => Connection -> Text -> m ()
logError' conn msg = liftIO $ void $ do
  putStrLn (unpack msg)
  time <- getCurrentTime
  execute conn "INSERT INTO `logs`(`message`,`timestamp`,`type`) VALUES (?,?,'error')" (msg, time)

logError :: (MonadIOReader m r, Has Connection r) => Text -> m ()
logError msg = do
  conn <- asks getter
  logError' conn msg

logErrorFork :: MonadIO m => Text -> m ()
logErrorFork msg = void $ liftIO $ do
  putStrLn (unpack msg)
  time <- getCurrentTime
  forkIO $ withDB $ \conn -> void $ execute conn "INSERT INTO `logs`(`message`,`timestamp`,`type`) VALUES (?,?,'error')" (msg, time)

replaceQuery :: Text -> Text -> Query -> Query
replaceQuery from to q = Query $ BS.toStrict $ BS.replace (T.encodeUtf8 from) (BS.fromStrict $ T.encodeUtf8 to :: BS.ByteString) (fromQuery q)

queryLog :: (QueryParams q, QueryResults r, MonadIOReader m rd, Has Connection rd) => Query -> q -> m [r]
queryLog q d = do
  conn <- asks getter
  trueQuery <- liftIO $ formatQuery conn q d
  logInfo' conn $ "Executing query: " <> showt trueQuery
  liftIO $ query conn q d

queryLog_ :: (QueryResults r, MonadIOReader m rd, Has Connection rd) => Query -> m [r]
queryLog_ q = do
  conn <- asks getter
  logInfo' conn $ "Executing query: " <> showt (fromQuery q)
  liftIO $ query_ conn q

queryOnlyLog :: (QueryParams q, QueryResults r, MonadIOReader m rd, Has Connection rd) => Query -> q -> m (Maybe r)
queryOnlyLog q d = do
  res <- queryLog q d
  when (length res > 1) $ logError "More query results than expected!"
  return $ only res

executeLog :: (QueryParams q, MonadIOReader m r, Has Connection r) => Query -> q -> m Int64
executeLog q d = do
  conn <- asks getter
  trueQuery <- liftIO $ formatQuery conn q d
  logInfo' conn $ "Executing query: " <> showt trueQuery
  liftIO $ execute conn q d

executeLog_ :: (MonadIOReader m r, Has Connection r) => Query -> m Int64
executeLog_ q = do
  conn <- asks getter
  logInfo' conn $ "Executing query: " <> showt (fromQuery q)
  liftIO $ execute_ conn q

executeManyLog :: (QueryParams q, MonadIOReader m r, Has Connection r) => Query -> [q] -> m Int64
executeManyLog q [] = do
  logInfo $ "Tried executing query with no data: " <> showt (fromQuery q)
  return 0
executeManyLog q d = do
  conn <- asks getter
  trueQuery <- liftIO $ formatMany conn q d
  logInfo' conn $ "Executing query: " <> showt trueQuery
  liftIO $ executeMany conn q d

withTransaction' :: (MonadHoistIO m) => Connection -> m a -> m a
withTransaction' conn a = do
  hoistIO (M.withTransaction conn) a

withTransaction :: (MonadHoistIOReader m r, Has Connection r) => m a -> m a
withTransaction a = do
  conn <- asks getter
  withTransaction' conn a

commit' :: (MonadIO m) => Connection -> m ()
commit' conn = liftIO $ M.commit conn

commit :: (MonadIOReader m r, Has Connection r) => m ()
commit = do
  conn <- asks getter
  commit' conn

rollback' :: (MonadIO m) => Connection -> m ()
rollback' conn = liftIO $ M.rollback conn

rollback :: (MonadIOReader m r, Has Connection r) => m ()
rollback = do
  conn <- asks getter
  rollback' conn

insertID' :: (MonadIO m, Num a) => Connection -> m a
insertID' conn = liftIO $ fromIntegral <$> M.insertID conn

insertID :: (MonadIOReader m r, Has Connection r, Num a) => m a
insertID = do
  conn <- asks getter
  insertID' conn

class QueryResults a => QueryResultsSize a where
  queryResultsSize :: Proxy a -> Int
instance Result a => QueryResultsSize (Only a) where
  queryResultsSize _ = 1
instance (Result a, Result b) => QueryResultsSize (a, b) where
  queryResultsSize _ = 2

newtype Concat tuple = Concat { fromConcat :: tuple } deriving (Show, Eq)

instance (QueryParams a, QueryParams b) => QueryParams (Concat (a, b)) where
  renderParams (Concat (a, b)) = renderParams a ++ renderParams b
instance (QueryResultsSize a, QueryResults b) => QueryResults (Concat (a, b)) where
  convertResults fields strings = let
    aSize = queryResultsSize (Proxy @a)
    (aFields, bFields) = splitAt aSize fields
    (aStrings, bStrings) = splitAt aSize strings
    a = convertResults aFields aStrings
    b = convertResults bFields bStrings
      in Concat (a, b)
instance (QueryResultsSize a, QueryResultsSize b) => QueryResultsSize (Concat (a, b)) where
  queryResultsSize _ = queryResultsSize (Proxy @a) + queryResultsSize (Proxy @b)

instance (QueryParams a, QueryParams b, QueryParams c) => QueryParams (Concat (a, b, c)) where
  renderParams (Concat (a, b, c)) = renderParams a ++ renderParams b ++ renderParams c
instance (QueryResultsSize a, QueryResultsSize b, QueryResults c) => QueryResults (Concat (a, b, c)) where
  convertResults fields strings = let
    aSize = queryResultsSize (Proxy @a)
    (aFields, bcFields) = splitAt aSize fields
    (aStrings, bcStrings) = splitAt aSize strings
    a = convertResults aFields aStrings
    bSize = queryResultsSize (Proxy @b)
    (bFields, cFields) = splitAt bSize bcFields
    (bStrings, cStrings) = splitAt bSize bcStrings
    b = convertResults bFields bStrings
    c = convertResults cFields cStrings
      in Concat (a, b, c)
instance (QueryResultsSize a, QueryResultsSize b, QueryResultsSize c) => QueryResultsSize (Concat (a, b, c)) where
  queryResultsSize _ = queryResultsSize (Proxy @a) + queryResultsSize (Proxy @b) + queryResultsSize (Proxy @c)