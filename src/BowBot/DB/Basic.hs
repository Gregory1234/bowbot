{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.DB.Basic(
  module BowBot.DB.Basic, Connection, Only(..), withTransaction, commit, rollback, insertID
) where

import BowBot.Utils
import Database.MySQL.Simple
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

logInfoDB :: MonadIO m => Connection -> String -> m ()
logInfoDB conn msg = liftIO $ void $ do
  putStrLn msg
  execute conn "INSERT INTO `logs`(`message`) VALUES (?)" (Only msg)

logInfo :: MonadIO m => String -> m ()
logInfo msg = void $ liftIO $ do
  putStrLn msg
  forkIO $ withDB $ \conn -> void $ execute conn "INSERT INTO `logs`(`message`) VALUES (?)" (Only msg)

logErrorDB :: MonadIO m => Connection -> String -> m ()
logErrorDB conn msg = liftIO $ void $ do
  putStrLn msg
  execute conn "INSERT INTO `logs`(`message`,`type`) VALUES (?,'error')" (Only msg)

logError :: MonadIO m => String -> m ()
logError msg = void $ liftIO $ do
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

queryLog :: (QueryParams q, QueryResults r, MonadIO m) => Connection -> Query -> q -> m [r]
queryLog conn q d = do
  dev <- ifDev "" (return "Dev")
  let nq = replaceQuery "DEV" dev q
  trueQuery <- liftIO $ formatQuery conn nq d
  logInfoDB conn $ "Executing query: " ++ show trueQuery
  liftIO $ query conn nq d

executeLog :: (QueryParams q, MonadIO m) => Connection -> Query -> q -> m Int64
executeLog conn q d = do
  dev <- ifDev "" (return "Dev")
  let nq = replaceQuery "DEV" dev q
  trueQuery <- liftIO $ formatQuery conn nq d
  logInfoDB conn $ "Executing query: " ++ show trueQuery
  liftIO $ execute conn nq d

executeManyLog :: (QueryParams q, MonadIO m) => Connection -> Query -> [q] -> m Int64
executeManyLog conn q [] = do
  dev <- ifDev "" (return "Dev")
  let nq = replaceQuery "DEV" dev q
  logInfoDB conn $ "Tried executing query with no data: " ++ show nq
  return 0
executeManyLog conn q d = do
  dev <- ifDev "" (return "Dev")
  let nq = replaceQuery "DEV" dev q
  trueQuery <- liftIO $ formatMany conn nq d
  logInfoDB conn $ "Executing query: " ++ show trueQuery
  liftIO $ executeMany conn nq d
  
