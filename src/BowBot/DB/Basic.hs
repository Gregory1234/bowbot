{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.DB.Basic(
  module BowBot.DB.Basic, Connection, Only(..)
) where

import BowBot.Utils
import Database.MySQL.Simple hiding (withTransaction, commit, rollback, insertID)
import qualified Database.MySQL.Simple as M (withTransaction, commit, rollback, insertID)
import Database.MySQL.Simple.QueryParams (QueryParams)
import Database.MySQL.Simple.QueryResults (QueryResults)
import Database.MySQL.Simple.Types (Query(..))
import Data.Int (Int64)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Search as BS
import Data.String (fromString)
import Control.Exception.Base (bracket)
import Control.Concurrent (forkIO)


withDB :: MonadHoistIO m => (Connection -> m a) -> m a -- TODO: report connection errors
withDB f = do
  -- liftIO $ putStrLn "Start connection"
  connectHost <- liftIO $ getEnvOrThrow "DB_HOST"
  connectUser <- liftIO $ getEnvOrThrow "DB_USER"
  connectPassword <- liftIO $ getEnvOrThrow "DB_PASS"
  connectDatabase <- liftIO $ getEnvOrThrow "DB_NAME"
  hoistIOWithArg (bracket (liftIO $ connect $ defaultConnectInfo { connectHost, connectUser, connectPassword, connectDatabase }) close) f

logInfo' :: MonadIO m => Connection -> String -> m ()
logInfo' conn msg = liftIO $ void $ do
  putStrLn msg
  execute conn "INSERT INTO `logs`(`message`) VALUES (?)" (Only msg)

logInfo :: (MonadIOReader m r, Has Connection r) => String -> m ()
logInfo msg = do
  conn <- asks getter
  logInfo' conn msg

logInfoFork :: MonadIO m => String -> m ()
logInfoFork msg = void $ liftIO $ do
  putStrLn msg
  forkIO $ withDB $ \conn -> void $ execute conn "INSERT INTO `logs`(`message`) VALUES (?)" (Only msg)

logError' :: MonadIO m => Connection -> String -> m ()
logError' conn msg = liftIO $ void $ do
  putStrLn msg
  execute conn "INSERT INTO `logs`(`message`,`type`) VALUES (?,'error')" (Only msg)

logError :: (MonadIOReader m r, Has Connection r) => String -> m ()
logError msg = do
  conn <- asks getter
  logError' conn msg

logErrorFork :: MonadIO m => String -> m ()
logErrorFork msg = void $ liftIO $ do
  putStrLn msg
  forkIO $ withDB $ \conn -> void $ execute conn "INSERT INTO `logs`(`message`,`type`) VALUES (?,'error')" (Only msg)

replaceQuery :: String -> String -> Query -> Query
replaceQuery from to q = Query $ BS.toStrict $ BS.replace (fromString from) (fromString to :: BS.ByteString) (fromQuery q)

optionalQueryFilters :: [(String, Bool)] -> Query -> Query
optionalQueryFilters [] q = q
optionalQueryFilters as q = Query $ (<>fromString ("WHERE " ++ intercalate " AND " (helper <$> as))) $ fromQuery q
  where
    helper (a, True) = a ++ " = ?"
    helper (a, False) = "(1 OR " ++ a ++ " = ?)"

queryLog' :: (QueryParams q, QueryResults r, MonadIO m) => Connection -> Query -> q -> m [r]
queryLog' conn q d = do
  dev <- ifDev "" (return "Dev")
  let nq = replaceQuery "DEV" dev q
  trueQuery <- liftIO $ formatQuery conn nq d
  logInfo' conn $ "Executing query: " ++ show trueQuery
  liftIO $ query conn nq d

executeLog' :: (QueryParams q, MonadIO m) => Connection -> Query -> q -> m Int64
executeLog' conn q d = do
  dev <- ifDev "" (return "Dev")
  let nq = replaceQuery "DEV" dev q
  trueQuery <- liftIO $ formatQuery conn nq d
  logInfo' conn $ "Executing query: " ++ show trueQuery
  liftIO $ execute conn nq d

executeManyLog' :: (QueryParams q, MonadIO m) => Connection -> Query -> [q] -> m Int64
executeManyLog' conn q [] = do
  dev <- ifDev "" (return "Dev")
  let nq = replaceQuery "DEV" dev q
  logInfo' conn $ "Tried executing query with no data: " ++ show nq
  return 0
executeManyLog' conn q d = do
  dev <- ifDev "" (return "Dev")
  let nq = replaceQuery "DEV" dev q
  trueQuery <- liftIO $ formatMany conn nq d
  logInfo' conn $ "Executing query: " ++ show trueQuery
  liftIO $ executeMany conn nq d

queryLog :: (QueryParams q, QueryResults r, MonadIOReader m rd, Has Connection rd) => Query -> q -> m [r]
queryLog q d = do
  conn <- asks getter
  queryLog' conn q d

executeLog :: (QueryParams q, MonadIOReader m r, Has Connection r) => Query -> q -> m Int64
executeLog q d = do
  conn <- asks getter
  executeLog' conn q d

executeManyLog :: (QueryParams q, MonadIOReader m r, Has Connection r) => Query -> [q] -> m Int64
executeManyLog q d = do
  conn <- asks getter
  executeManyLog' conn q d

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