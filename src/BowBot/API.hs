{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingVia #-}

module BowBot.API(
  module BowBot.API, module BowBot.Utils, (.:), (.:?), (.!=), Object, Parser, Manager
) where

import Network.HTTP.Conduit hiding (path)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Base64.URL (encodeBase64)
import Control.Exception.Base (try, SomeException, evaluate)
import Data.ByteString.Lazy (ByteString)
import BowBot.Utils
import Data.Aeson
import Data.Aeson.Types
import Control.Concurrent (forkIO, threadDelay)
import Control.DeepSeq (force)
import Control.Monad.Reader (ReaderT(..), MonadTrans)

managerSettings :: ManagerSettings
managerSettings = tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro 15000000 }

class MonadIO m => APIMonad m where
  hManager :: m Manager

newtype ManagerT m a = ManagerT { runManagerT :: Manager -> m a }
  deriving (Functor, Applicative, Monad, MonadIO) via (ReaderT Manager m)
  deriving (MonadTrans) via (ReaderT Manager)

instance MonadIO m => APIMonad (ManagerT m) where
  hManager = ManagerT return

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

hLogInfo :: APIMonad m => String -> m ()
hLogInfo msg = do
  man <- hManager
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

hLogError :: APIMonad m => String -> m ()
hLogError msg = do
  man <- hManager
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
    (Right v) -> do
      logInfo manager $ "Received response from: " ++ cleanUrl
      return $ responseBody v

hSendRequestTo :: APIMonad m => String -> String -> m ByteString
hSendRequestTo u c = do
  man <- hManager
  liftIO $ sendRequestTo man u c

getInfoDB :: Manager -> String -> IO (Maybe String)
getInfoDB man name = do
  res <- sendDB man "info/get.php" ["name=" ++ name]
  decodeParse res $ \o -> o .: "value"

hInfoDB :: APIMonad m => String -> m (Maybe String)
hInfoDB n = do
  man <- hManager
  liftIO $ getInfoDB man n

sendDB :: Manager -> String -> [String] -> IO ByteString
sendDB manager path args = do
  website <- fromMaybe "" <$> getEnv "DB_SITE"
  apiKey <- fromMaybe "" <$> getEnv "DB_KEY"
  dev <- ifDev "" (return "&dev")
  let url = "http://" ++ website ++ "/api/" ++ path ++ "?key=" ++ apiKey ++ (('&':) =<< args) ++ dev
  let cleanUrl = "http://[REDACTED]/api/" ++ path ++ "?key=[REDACTED]" ++ (('&':) =<< args) ++ dev
  sendRequestTo manager url cleanUrl

hSendDB :: APIMonad m => String -> [String] -> m ByteString
hSendDB p a = do
  man <- hManager
  liftIO $ sendDB man p a

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

hPostDB :: APIMonad m => String -> Value -> m ()
hPostDB p v = do
  man <- hManager
  liftIO $ sendPostDB man p v

decodeParse :: (FromJSON o, MonadIO m) => ByteString -> (o -> Parser a) -> m (Maybe a)
decodeParse (decode -> Just str) parser = case parseEither parser str of
  Left e -> do
    logError' $ show e
    return Nothing
  Right a -> return $ Just a
decodeParse str _ = do
  logError' $ "Decoding failed in " ++ show str ++ "!"
  return Nothing