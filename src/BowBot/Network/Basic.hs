{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BowBot.Network.Basic where


import Network.HTTP.Conduit hiding (path)
import Data.Aeson.Types (FromJSON, Parser, parseEither)
import Data.ByteString.Lazy.Char8 (ByteString)
import Control.Exception.Base (SomeException, evaluate, try)
import Control.DeepSeq (force)
import BowBot.DB.Basic
import Control.Concurrent (threadDelay)
import Data.Aeson (decode)
import BowBot.Utils (MonadIO(..))



managerSettings :: ManagerSettings
managerSettings = tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro 15000000 }

sendRequestTo :: Manager -> String -> String -> IO ByteString
sendRequestTo manager url cleanUrl = do
  _ <- evaluate $ force url
  _ <- evaluate $ force cleanUrl
  logInfo cleanUrl
  request <- parseRequest url
  res <- try $ httpLbs request manager
  case res of
    (Left (e :: SomeException)) -> do
      logError $ show e
      threadDelay 3000000
      sendRequestTo manager url cleanUrl
    (Right v) -> do
      logInfo $ "Received response from: " ++ cleanUrl
      return $ responseBody v

decodeParse :: (FromJSON o, MonadIO m) => ByteString -> (o -> Parser a) -> m (Maybe a)
decodeParse (decode -> Just str) parser = liftIO $ case parseEither parser str of
  Left e -> do
    logError $ show e
    return Nothing
  Right a -> return $ Just a
decodeParse str _ = liftIO $ do
  logError $ "Decoding failed in " ++ show str ++ "!"
  return Nothing