{-# LANGUAGE QuasiQuotes #-}

module BowBot.DB.Basic(
  module BowBot.DB.Basic, module Language.MySQL.Quasi, Connection, Q.Param, Q.Result, Q.ToField(..), textSqlTypes, Q.FromField(..), ToMysql(..), FromMysql(..), StateT(..), SimpleValue(..), Generic(..), Generically(..)
) where

import BowBot.Utils
import Database.MySQL.Base (ConnectInfo(..), connect, close, defaultConnectInfo)
import qualified Database.MySQL.Simple as Q
import qualified Database.MySQL.Simple.Types as Q
import Data.Int (Int64)
import Control.Exception.Base (bracket)
import Control.Concurrent (forkIO)
import Data.Time (getCurrentTime, UTCTime)
import Language.MySQL.Query
import Language.MySQL.Quasi
import Data.Coerce (coerce)
import Data.ByteString
import qualified Data.Text.IO as T


withDB :: MonadHoistIO m => (Connection -> m a) -> m a -- TODO: report connection errors
withDB f = do
  -- liftIO $ putStrLn "Start connection"
  connectHost <- liftIO $ getEnvOrThrow "DB_HOST"
  connectUser <- liftIO $ getEnvOrThrow "DB_USER"
  connectPassword <- liftIO $ getEnvOrThrow "DB_PASS"
  connectDatabase <- liftIO $ getEnvOrThrow "DB_NAME"
  hoistIOWithArg (bracket (liftIO $ connect $ defaultConnectInfo { connectHost, connectUser, connectPassword, connectDatabase }) close) f

query' :: forall r. FromMysql r => Connection -> RenderedQuery r -> IO [r]
query' conn (RenderedQuery q) = coerce $ Q.query_ @(Flattened r) conn (Q.Query q)

execute' :: Connection -> RenderedCommand -> IO (Maybe Int64)
execute' _ (RenderedCommand Nothing) = pure Nothing
execute' conn (RenderedCommand (Just q)) = Just <$> Q.execute_ conn (Q.Query q)

withoutLogging :: (MonadIOReader m rd, Has Connection rd) => (Connection -> q -> IO r) -> (Connection -> IO q) -> m r
withoutLogging exec q = do
  conn <- asks getter
  q' <- liftIO $ q conn
  liftIO $ exec conn q'

query :: (MonadIOReader m rd, Has Connection rd, FromMysql r) => Query r -> m [r]
query = withoutLogging query'

execute :: (MonadIOReader m r, Has Connection r) => Command -> m Int64
execute = fmap (fromMaybe 0) . withoutLogging execute'

-- TODO: remove repetition

logInfo :: (MonadIOReader m r, Has Connection r) => Text -> m ()
logInfo msg = do
  liftIO $ T.putStrLn msg
  time <- liftIO getCurrentTime
  void $ execute [mysql|INSERT INTO `logs`(`message`, `timestamp`) VALUES (msg, time)|]

logError :: (MonadIOReader m r, Has Connection r) => Text -> m ()
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

withLogging :: (MonadIOReader m rd, Has Connection rd) => (Connection -> q -> IO r) -> (q -> Maybe ByteString) -> (Connection -> IO q) -> m r
withLogging exec toShow q = do
  conn <- asks getter
  q' <- liftIO $ q conn
  logInfo $ case toShow q' of
    Nothing -> "Tried executing query with no data!"
    Just str -> "Executing query: " <> showt str
  liftIO $ exec conn q'

queryLog :: (MonadIOReader m rd, Has Connection rd, FromMysql r) => Query r -> m [r]
queryLog = withLogging query' (\(RenderedQuery q) -> Just q)

queryOnlyLog :: (MonadIOReader m rd, Has Connection rd, FromMysql r) => Query r -> m (Maybe r)
queryOnlyLog = fmap only . queryLog

executeLog :: (MonadIOReader m rd, Has Connection rd) => Command -> m Int64
executeLog = fmap (fromMaybe 0) . withLogging execute' (\(RenderedCommand q) -> q)

withTransaction :: (MonadHoistIOReader m r, Has Connection r) => m a -> m a
withTransaction a = do
  conn <- asks getter
  hoistIO (Q.withTransaction conn) a

commit :: (MonadIOReader m r, Has Connection r) => m ()
commit = do
  conn <- asks getter
  liftIO $ Q.commit conn

rollback :: (MonadIOReader m r, Has Connection r) => m ()
rollback = do
  conn <- asks getter
  liftIO $ Q.rollback conn

insertID :: (MonadIOReader m r, Has Connection r, Num a) => m a
insertID = do
  conn <- asks getter
  liftIO $ fromIntegral <$> Q.insertID conn
