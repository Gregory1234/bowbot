{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.DB(
  module BowBot.DB, module Database.MySQL.Simple
) where

import Database.MySQL.Simple
import Database.MySQL.Simple.QueryParams
import Database.MySQL.Simple.QueryResults
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Search as BS
import Database.MySQL.Simple.Types (Query(..))
import BowBot.CommandMonads
import BowBot.Utils

withDB :: MonadIO m => (Connection -> m a) -> m a
withDB f = do
  connectHost <- liftIO $ fromMaybe "" <$> getEnv "DB_HOST"
  connectUser <- liftIO $ fromMaybe "" <$> getEnv "DB_USER"
  connectPassword <- liftIO $ fromMaybe "" <$> getEnv "DB_PASS"
  connectDatabase <- liftIO $ fromMaybe "" <$> getEnv "DB_NAME"
  conn <- liftIO $ connect $ defaultConnectInfo { connectHost, connectUser, connectPassword, connectDatabase }
  r <- f conn
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

queryLog :: (QueryParams q, QueryResults r, MonadIO m) => Connection -> Query -> q -> m [r]
queryLog conn q d = do
  dev :: BS.ByteString <- ifDev "" (return "Dev")
  let nq = Query $ BS.toStrict $ BS.replace "DEV" dev (fromQuery q)
  trueQuery <- liftIO $ formatQuery conn nq d
  logInfoDB conn $ "Executing query: " ++ show trueQuery
  liftIO $ query conn nq d

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