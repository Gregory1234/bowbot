{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module BowBot.API(
  module BowBot.API, module BowBot.Utils, (.:), (.:?), (.!=), Manager
) where

import Network.HTTP.Conduit
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Base64.URL (encodeBase64)
import Control.Exception.Base (try, SomeException, evaluate)
import Data.ByteString.Lazy (ByteString)
import BowBot.Utils
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither, (.:), (.:?), (.!=))
import Control.Concurrent (forkIO, threadDelay)
import Control.DeepSeq (force)

managerSettings :: ManagerSettings
managerSettings = tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro 15000000 }

-- TODO: create a logger monad

logInfo :: MonadIO m => Manager -> String -> m ()
logInfo man msg = liftIO $ void $ forkIO $ do
  _ <- evaluate $ force msg
  putStrLn msg
  website <- fromMaybe "" <$> getEnv "DB_SITE"
  apiKey <- fromMaybe "" <$> getEnv "DB_KEY"
  let url = "http://" ++ website ++ "/api/log/info.php?key=" ++ apiKey ++ "&msg=" ++ unpack (encodeBase64 (BS.pack msg))
  request <- parseRequest url
  void $ try @SomeException $ httpLbs request man

logInfo' :: MonadIO m => String -> m ()
logInfo' msg = do
  man <- liftIO $ newManager managerSettings
  logInfo man msg

logError :: MonadIO m => Manager -> String -> m ()
logError man msg = liftIO $ void $ forkIO $ do
  _ <- evaluate $ force msg
  putStrLn msg
  website <- fromMaybe "" <$> getEnv "DB_SITE"
  apiKey <- fromMaybe "" <$> getEnv "DB_KEY"
  let url = "http://" ++ website ++ "/api/log/err.php?key=" ++ apiKey ++ "&msg=" ++ unpack (encodeBase64 (BS.pack msg))
  request <- parseRequest url
  void $ try @SomeException $ httpLbs request man

logError' :: MonadIO m => String -> m ()
logError' msg = do
  man <- liftIO $ newManager managerSettings
  logError man msg

sendRequestTo :: Manager -> String -> String -> IO ByteString
sendRequestTo manager url cleanUrl = do
  _ <- evaluate $ force url
  _ <- evaluate $ force cleanUrl
  logInfo manager cleanUrl
  request <- parseRequest url
  res <- try $ httpLbs request manager
  case res of
    (Left (e :: SomeException)) -> do
      logError manager $ show e
      threadDelay 3000000
      sendRequestTo manager url cleanUrl
    (Right v) -> return $ responseBody v

sendDB :: Manager -> String -> [String] -> IO ByteString
sendDB manager path args = do
  website <- fromMaybe "" <$> getEnv "DB_SITE"
  apiKey <- fromMaybe "" <$> getEnv "DB_KEY"
  dev <- ifDev "" (return "&dev")
  let url = "http://" ++ website ++ "/api/" ++ path ++ "?key=" ++ apiKey ++ (('&':) =<< args) ++ dev
  let cleanUrl = "http://[REDACTED]/api/" ++ path ++ "?key=[REDACTED]" ++ (('&':) =<< args) ++ dev
  res <- sendRequestTo manager url cleanUrl
  logInfo manager $ "Received response from: " ++ cleanUrl
  return res

sendPostDB :: Manager -> String -> Value -> IO ()
sendPostDB manager path dat = do
  _ <- evaluate $ force path
  _ <- evaluate $ force dat
  website <- fromMaybe "" <$> getEnv "DB_SITE"
  apiKey <- fromMaybe "" <$> getEnv "DB_KEY"
  dev <- ifDev "" (return "&dev")
  let url = "http://" ++ website ++ "/api/" ++ path ++ "?key=" ++ apiKey ++ dev
  let cleanUrl = "http://[REDACTED]/api/" ++ path ++ "?key=[REDACTED]" ++ dev
  logInfo manager cleanUrl
  initRequest <- parseRequest url
  let request = initRequest { method = "POST", requestBody = RequestBodyLBS (encode dat) }
  void $ try @SomeException $ httpLbs request manager

decodeParse :: FromJSON o => ByteString -> (o -> Parser a) -> IO (Maybe a)
decodeParse (decode -> Just str) parser = case parseEither parser str of
  Left e -> do
    logError' $ show e
    return Nothing
  Right a -> return $ Just a
decodeParse str _ = do
  logError' $ "Decoding failed in " ++ show str ++ "!"
  return Nothing