{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BowBot.Network.Basic where


import Network.HTTP.Conduit hiding (path)
import Data.Aeson.Types (FromJSON, Parser, parseEither)
import Data.ByteString.Lazy.Char8 (ByteString)
import Control.Exception.Base (SomeException, evaluate, try)
import Database.MySQL.Simple (Connection)
import Control.DeepSeq (force)
import BowBot.DB.Basic
import BowBot.DB.Class
import Control.Concurrent (threadDelay)
import Data.Aeson (decode)



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


decodeParse :: (FromJSON o, MonadDB m) => ByteString -> (o -> Parser a) -> m (Maybe a)
decodeParse (decode -> Just str) parser = case parseEither parser str of
  Left e -> do
    hLogErrorDB $ show e
    return Nothing
  Right a -> return $ Just a
decodeParse str _ = do
  hLogErrorDB $ "Decoding failed in " ++ show str ++ "!"
  return Nothing