{-# LANGUAGE NamedFieldPuns #-}

module BowBot.DB(
  module BowBot.DB, module Database.MySQL.Simple
) where

import Database.MySQL.Simple
import Data.Maybe (fromMaybe)
import System.Environment.Blank (getEnv)
  
dbConnect :: IO Connection
dbConnect = do
  connectHost <- fromMaybe "" <$> getEnv "DB_HOST"
  connectUser <- fromMaybe "" <$> getEnv "DB_USER"
  connectPassword <- fromMaybe "" <$> getEnv "DB_PASS"
  connectDatabase <- fromMaybe "" <$> getEnv "DB_NAME"
  connect $ defaultConnectInfo { connectHost, connectUser, connectPassword, connectDatabase }