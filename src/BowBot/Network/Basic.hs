{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.Network.Basic(
  module BowBot.Network.Basic, Manager, newManager, MonadIO(..), MonadReader(..), asks, Has(..),
  module Data.Aeson.Types, module Data.Aeson
) where


import Network.HTTP.Conduit hiding (path)
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 (ByteString)
import Control.Exception.Base (SomeException, evaluate, try)
import Control.DeepSeq (force)
import BowBot.DB.Basic
import Control.Concurrent (threadDelay)
import Data.Aeson
import Control.Monad.Reader
import BowBot.Utils



managerSettings :: ManagerSettings
managerSettings = tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro 15000000 }

sendRequestTo' :: Manager -> Text -> Text -> IO ByteString
sendRequestTo' manager url cleanUrl = do
  _ <- evaluate $ force url
  _ <- evaluate $ force cleanUrl
  logInfoFork cleanUrl
  request <- parseRequest (unpack url)
  res <- try $ httpLbs request manager
  case res of
    (Left (e :: SomeException)) -> do
      logErrorFork $ showt e
      threadDelay 3000000
      sendRequestTo' manager url cleanUrl
    (Right v) -> do
      logInfoFork $ "Received response from: " <> cleanUrl
      return $ responseBody v

sendRequestTo :: (MonadIOReader m r, Has Manager r) => Text -> Text -> m ByteString
sendRequestTo url cleanUrl = do
  manager <- asks getter
  liftIO $ sendRequestTo' manager url cleanUrl

decodeParse :: (FromJSON o, MonadIO m) => ByteString -> (o -> Parser a) -> m (Maybe a)
decodeParse (decode -> Just str) parser = liftIO $ case parseEither parser str of
  Left e -> do
    logErrorFork $ showt e
    return Nothing
  Right a -> return $ Just a
decodeParse str _ = liftIO $ do
  logErrorFork $ "Decoding failed in " <> showt str <> "!"
  return Nothing