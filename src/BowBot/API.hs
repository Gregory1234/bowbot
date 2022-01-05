{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingVia #-}

module BowBot.API(
  module BowBot.API, module BowBot.Utils, (.:), (.:?), (.!=), Object, Parser, Manager, module BowBot.CommandMonads
) where

import Network.HTTP.Conduit hiding (path)
import Control.Exception.Base (try, SomeException, evaluate)
import Data.ByteString.Lazy (ByteString)
import BowBot.Utils
import Data.Aeson
import Data.Aeson.Types
import Control.Concurrent (threadDelay)
import Control.DeepSeq (force)
import BowBot.CommandMonads (APIMonad(..), ManagerT(..))
import BowBot.DB

managerSettings :: ManagerSettings
managerSettings = tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro 15000000 }

sendRequestTo :: Connection -> Manager -> String -> String -> IO ByteString
sendRequestTo conn manager url cleanUrl = do
  _ <- evaluate $ force url
  _ <- evaluate $ force cleanUrl
  logInfoDB conn cleanUrl
  request <- parseRequest url
  res <- try $ httpLbs request manager
  case res of
    (Left (e :: SomeException)) -> do
      logErrorDB conn $ show e
      threadDelay 3000000
      sendRequestTo conn manager url cleanUrl
    (Right v) -> do
      logInfoDB conn $ "Received response from: " ++ cleanUrl
      return $ responseBody v

hSendRequestTo :: APIMonad m => String -> String -> m ByteString
hSendRequestTo u c = do
  man <- hManager
  liftIO $ withDB $ \conn -> sendRequestTo conn man u c

decodeParse :: (FromJSON o, MonadIO m) => ByteString -> (o -> Parser a) -> m (Maybe a)
decodeParse (decode -> Just str) parser = case parseEither parser str of
  Left e -> do
    withDB $ flip logErrorDB $ show e
    return Nothing
  Right a -> return $ Just a
decodeParse str _ = do
  withDB $ flip logErrorDB $ "Decoding failed in " ++ show str ++ "!"
  return Nothing