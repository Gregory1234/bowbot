{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.DB(
  module BowBot.DB, module Database.MySQL.Simple, module BowBot.CommandMonads
) where

import Database.MySQL.Simple
import Database.MySQL.Simple.QueryParams
import Database.MySQL.Simple.QueryResults
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Search as BS
import Database.MySQL.Simple.Types (Query(..))
import BowBot.CommandMonads (DBMonad(..))
import BowBot.Utils
import Data.Int (Int64)
import Data.String (fromString)

withDB :: MonadIO m => (Connection -> m a) -> m a -- TODO: report connection errors
withDB f = do
  connectHost <- liftIO $ fromMaybe "" <$> getEnv "DB_HOST"
  connectUser <- liftIO $ fromMaybe "" <$> getEnv "DB_USER"
  connectPassword <- liftIO $ fromMaybe "" <$> getEnv "DB_PASS"
  connectDatabase <- liftIO $ fromMaybe "" <$> getEnv "DB_NAME"
  conn <- liftIO $ connect $ defaultConnectInfo { connectHost, connectUser, connectPassword, connectDatabase }
  r <- f conn -- TODO: handle exceptions safely
  liftIO $ close conn
  return r

logInfoDB :: MonadIO m => Connection -> String -> m ()
logInfoDB conn msg = liftIO $ void $ do
  putStrLn msg
  execute conn "INSERT INTO `logs`(`message`) VALUES (?)" (Only msg)

hLogInfoDB :: DBMonad m => String -> m ()
hLogInfoDB str = hConnection >>= flip logInfoDB str

logErrorDB :: MonadIO m => Connection -> String -> m ()
logErrorDB conn msg = liftIO $ void $ do
  putStrLn msg
  execute conn "INSERT INTO `logs`(`message`,`type`) VALUES (?,'error')" (Only msg)

hLogErrorDB :: DBMonad m => String -> m ()
hLogErrorDB str = hConnection >>= flip logErrorDB str

replaceQuery :: String -> String -> Query -> Query
replaceQuery from to q = Query $ BS.toStrict $ BS.replace (fromString from) (fromString to :: BS.ByteString) (fromQuery q)

queryLog :: (QueryParams q, QueryResults r, MonadIO m) => Connection -> Query -> q -> m [r]
queryLog conn q d = do
  dev <- ifDev "" (return "Dev")
  let nq = replaceQuery "DEV" dev q
  trueQuery <- liftIO $ formatQuery conn nq d
  logInfoDB conn $ "Executing query: " ++ show trueQuery
  liftIO $ query conn nq d

hQueryLog :: (QueryParams q, QueryResults r, DBMonad m) => Query -> q -> m [r]
hQueryLog q d = do
  conn <- hConnection
  queryLog conn q d

executeLog :: (QueryParams q, MonadIO m) => Connection -> Query -> q -> m Int64
executeLog conn q d = do
  dev <- ifDev "" (return "Dev")
  let nq = replaceQuery "DEV" dev q
  trueQuery <- liftIO $ formatQuery conn nq d
  logInfoDB conn $ "Executing query: " ++ show trueQuery
  liftIO $ execute conn nq d

hExecuteLog :: (QueryParams q, DBMonad m) => Query -> q -> m Int64
hExecuteLog q d = do
  conn <- hConnection
  executeLog conn q d

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

hExecuteManyLog :: (QueryParams q, DBMonad m) => Query -> [q] -> m Int64
hExecuteManyLog q d = do
  conn <- hConnection
  executeManyLog conn q d

getInfoDB :: MonadIO m => Connection -> String -> m (Maybe String)
getInfoDB conn name = do
  xs :: [Only String] <- queryLog conn "SELECT `value` FROM `botInfoDEV` WHERE `name` = ?" (Only name)
  case xs of
    [Only r] -> return $ Just r
    _ -> do
      logErrorDB conn $ "Info not found: " ++ name
      return Nothing

hInfoDB :: DBMonad m => String -> m (Maybe String)
hInfoDB name = do
  conn <- hConnection
  getInfoDB conn name