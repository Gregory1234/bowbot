{-# LANGUAGE QuasiQuotes #-}

module BowBot.DB.Basic(
  module BowBot.DB.Basic, module Language.MySQL.Quasi, ToMysqlSimple(..), FromMysqlSimple(..), ToMysql(..), FromMysql(..), MysqlEnum(..), MysqlAutoIncrement(..), StateT(..), SimpleValue(..), EnumValue(..), Generic(..), Generically(..)
) where

import BowBot.Utils
import Database.MySQL.Base (MySQLConn, ConnectInfo(..), connect, close, defaultConnectInfo)
import qualified Database.MySQL.Base as Q
import Control.Exception.Base (bracket)
import Control.Concurrent (forkIO)
import Data.Time (getCurrentTime, UTCTime)
import Language.MySQL.Query
import Language.MySQL.Quasi
import Data.ByteString (ByteString)
import qualified Data.Text.IO as T
import Control.Concurrent.MVar
import qualified Data.ByteString.Char8 as BS
import qualified System.IO.Streams as S
import Control.Monad.State.Strict (evalState)
import Control.Exception (onException)

newtype SafeMysqlConn = SafeMysqlConn { safeMysqlConnVar :: MVar MySQLConn }

withDB :: MonadHoistIO m => (SafeMysqlConn -> m a) -> m a -- TODO: report connection errors
withDB f = do
  -- liftIO $ putStrLn "Start connection"
  ciHost <- liftIO $ getEnvOrThrow "DB_HOST"
  ciUser <- liftIO $ BS.pack <$> getEnvOrThrow "DB_USER"
  ciPassword <- liftIO $ BS.pack <$> getEnvOrThrow "DB_PASS"
  ciDatabase <- liftIO $ BS.pack <$> getEnvOrThrow "DB_NAME"
  hoistIOWithArg (bracket (liftIO $ connect defaultConnectInfo { ciHost, ciUser, ciPassword, ciDatabase }) close) $ \conn -> do
    safeMysqlConnVar <- liftIO $ newMVar conn
    f SafeMysqlConn {..}

streamToList :: S.InputStream a -> IO [a]
streamToList s = do
  v' <- S.read s
  case v' of
    Nothing -> return []
    Just v -> (v:) <$> streamToList s

withSafeConn :: (MonadIOReader m r, Has SafeMysqlConn r) => (MySQLConn -> IO a) -> m a
withSafeConn f = do
  SafeMysqlConn {..} <- asks getter
  liftIO $ withMVar safeMysqlConnVar f

query :: (MonadIOReader m rd, Has SafeMysqlConn rd, FromMysql r) => Query r -> m [r]
query (Query q) = withSafeConn $ \conn -> do
  (_, valsStream) <- Q.query_ conn $ Q.Query $ BS.fromStrict q
  map (evalState rowParser) <$> streamToList valsStream

execute' :: (MonadIOReader m r, Has SafeMysqlConn r) => Command' -> m Int
execute' (Command q) = withSafeConn $ \conn -> do
  Q.OK {..} <- Q.execute_ conn $ Q.Query $ BS.fromStrict q
  return okAffectedRows

executeID :: (MonadIOReader m rd, Has SafeMysqlConn rd, MysqlAutoIncrement r) => CommandAI r -> m (Maybe r)
executeID (CommandAI q) = withSafeConn $ \conn -> do
  Q.OK {..} <- Q.execute_ conn $ Q.Query $ BS.fromStrict q
  return $ if okAffectedRows == 1 then Just $ fromAutoIncrement okLastInsertID else Nothing

execute :: (MonadIOReader m r, Has SafeMysqlConn r) => Command -> m Int
execute = either (const (pure 0)) execute'

-- TODO: remove repetition

logInfo :: (MonadIOReader m r, Has SafeMysqlConn r) => Text -> m ()
logInfo msg = do
  liftIO $ T.putStrLn msg
  time <- liftIO getCurrentTime
  void $ execute [mysql|INSERT INTO `logs`(`message`, `timestamp`) VALUES (msg, time)|]

logError :: (MonadIOReader m r, Has SafeMysqlConn r) => Text -> m ()
logError msg = do
  liftIO $ T.putStrLn msg
  time <- liftIO getCurrentTime
  void $ execute [mysql|INSERT INTO `logs`(`message`, `timestamp`,`type`) VALUES (msg, time, "error")|]

logInfoFork :: MonadIO m => Text -> m ()
logInfoFork msg = do
  liftIO $ T.putStrLn msg
  time <- liftIO getCurrentTime
  liftIO $ void $ forkIO $ withDB $ runReaderT $ void $ execute [mysql|INSERT INTO `logs`(`message`, `timestamp`) VALUES (msg, time)|]

logErrorFork :: MonadIO m => Text -> m ()
logErrorFork msg = do
  liftIO $ T.putStrLn msg
  time <- liftIO getCurrentTime
  liftIO $ void $ forkIO $ withDB $ runReaderT $ void $ execute [mysql|INSERT INTO `logs`(`message`, `timestamp`,`type`) VALUES (msg, time, "error")|]

withLogging :: (MonadIOReader m rd, Has SafeMysqlConn rd) => (SafeMysqlConn -> q -> IO r) -> (q -> ByteString) -> q -> m r
withLogging exec toShow q = do
  conn <- asks getter
  logInfo $ "Executing query: " <> showt (toShow q)
  liftIO $ exec conn q

queryLog :: (MonadIOReader m rd, Has SafeMysqlConn rd, FromMysql r) => Query r -> m [r]
queryLog q = do
  logInfo $ "Executing query: " <> showt (fromQuery q)
  query q

queryOnlyLog :: (MonadIOReader m rd, Has SafeMysqlConn rd, FromMysql r) => Query r -> m (Maybe r)
queryOnlyLog = fmap only . queryLog

executeLog :: (MonadIOReader m rd, Has SafeMysqlConn rd) => Command -> m Int
executeLog (Left q) = 0 <$ logInfo ("Tried executing query with no data: " <> showt q)
executeLog (Right q) = do
  logInfo $ "Executing command: " <> showt (fromCommand q)
  execute' q

executeIDLog :: (MonadIOReader m rd, Has SafeMysqlConn rd, MysqlAutoIncrement r) => CommandAI r -> m (Maybe r)
executeIDLog q = do
  logInfo $ "Executing command: " <> showt (fromCommandAI q)
  executeID q

withTransaction :: (MonadHoistIOReader m r, Has SafeMysqlConn r) => m a -> m a
withTransaction a = do
  _ <- withSafeConn $ \conn -> Q.execute_ conn "BEGIN"
  SafeMysqlConn {..} <- asks getter
  let rollback' = do
        bracket (takeMVar safeMysqlConnVar) (putMVar safeMysqlConnVar) $ \conn -> Q.execute_ conn "ROLLBACK"
  ret <- hoistIO (`onException` rollback') a
  _ <- withSafeConn $ \conn -> Q.execute_ conn "COMMIT"
  return ret

commit :: (MonadIOReader m r, Has SafeMysqlConn r) => m ()
commit = withSafeConn $ \conn -> void $ Q.execute_ conn "COMMIT"

rollback :: (MonadIOReader m r, Has SafeMysqlConn r) => m ()
rollback = withSafeConn $ \conn -> void $ Q.execute_ conn "ROLLBACK"
