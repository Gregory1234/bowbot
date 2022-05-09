{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.Network.ClearLogs where

import Control.Exception.Base (SomeException, try)
import BowBot.Utils
import Network.HTTP.Conduit hiding (path)
import Data.Has
import Control.Monad.RWS.Class

clearLogs :: (MonadReader r m, Has Manager r, MonadIO m) => m ()
clearLogs = asks getter >>= \man -> liftIO $ do
  website <- getEnvOrThrow "DB_SITE"
  apiKey <- getEnvOrThrow "DB_KEY"
  let url = "http://" ++ website ++ "/api/log/clear.php?key=" ++ apiKey
  request <- parseRequest url
  void $ try @SomeException $ httpLbs request man