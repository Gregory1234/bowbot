{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.DB(
  module BowBot.DB, module Database.MySQL.Simple
) where

import Database.MySQL.Simple
import Database.MySQL.Simple.QueryParams
import Database.MySQL.Simple.QueryResults
import BowBot.API
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Search as BS
import Database.MySQL.Simple.Types (Query(..))

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

queryLog :: (QueryParams q, QueryResults r, MonadIO m) => Connection -> Query -> q -> m [r]
queryLog conn q d = do
  dev :: BS.ByteString <- ifDev "" (return "Dev")
  let nq = Query $ BS.toStrict $ BS.replace "DEV" dev (fromQuery q)
  trueQuery <- liftIO $ formatQuery conn nq d
  logInfo' $ "Executing query: " ++ show trueQuery
  liftIO $ query conn nq d

getInfoDB :: MonadIO m => Connection -> String -> m (Maybe String)
getInfoDB conn name = do
  xs :: [Only String] <- queryLog conn "SELECT `value` FROM `botInfoDEV` WHERE `name` = ?" (Only name)
  case xs of
    [Only r] -> return $ Just r
    _ -> do
      logError' $ "Info not found: " ++ name
      return Nothing