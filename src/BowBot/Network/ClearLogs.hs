module BowBot.Network.ClearLogs where

import Control.Exception.Base (SomeException, try)
import BowBot.Utils
import Network.HTTP.Conduit hiding (path)

clearLogs :: (MonadIOReader m r, Has Manager r) => m ()
clearLogs = asks getter >>= \man -> liftIO $ do
  website <- getEnvOrThrow "DB_SITE"
  apiKey <- getEnvOrThrow "DB_KEY"
  let url = "http://" ++ website ++ "/api/log/clear.php?key=" ++ apiKey
  request <- parseRequest url
  void $ try @SomeException $ httpLbs request man