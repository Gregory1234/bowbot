{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module BowBot.API(
  module BowBot.API, module BowBot.Utils, (.:), (.:?), (.!=), Manager
) where

import Network.HTTP.Conduit
import Control.Exception.Base (try, SomeException)
import Data.ByteString.Lazy (ByteString)
import BowBot.Utils
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither, (.:), (.:?), (.!=))

managerSettings :: ManagerSettings
managerSettings = tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro 15000000 }

sendRequestTo :: Manager -> String -> String -> IO ByteString
sendRequestTo manager url cleanUrl = do
  putStrLn cleanUrl
  request <- parseRequest url
  res <- try $ httpLbs request manager
  case res of
    (Left (e :: SomeException)) -> do
      print e
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
  putStrLn $ "Received response from: " ++ cleanUrl
  return res

sendPostDB :: Manager -> String -> Value -> IO ()
sendPostDB manager path dat = do
  website <- fromMaybe "" <$> getEnv "DB_SITE"
  apiKey <- fromMaybe "" <$> getEnv "DB_KEY"
  dev <- ifDev "" (return "&dev")
  let url = "http://" ++ website ++ "/api/" ++ path ++ "?key=" ++ apiKey ++ dev
  let cleanUrl = "http://[REDACTED]/api/" ++ path ++ "?key=[REDACTED]" ++ dev
  putStrLn cleanUrl
  initRequest <- parseRequest url
  let request = initRequest { method = "POST", requestBody = RequestBodyLBS (encode dat) }
  void $ try @SomeException $ httpLbs request manager

decodeParse :: FromJSON o => ByteString -> (o -> Parser a) -> IO (Maybe a)
decodeParse (decode -> Just str) parser = case parseEither parser str of
  Left e -> do
    print e
    return Nothing
  Right a -> return $ Just a
decodeParse str _ = do
  putStrLn $ "Decoding failed in " ++ show str ++ "!"
  return Nothing