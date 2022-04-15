{-# LANGUAGE TypeApplications #-}

module BowBot.Network.ClearLogs where

import BowBot.Network.Class
import Control.Exception.Base (SomeException, try)
import BowBot.Utils
import Network.HTTP.Conduit hiding (path)
import BowBot.Command

clearLogs :: MonadNetwork m => m ()
clearLogs = hManager >>= \man -> liftIO $ do
  website <- getEnvOrThrow "DB_SITE"
  apiKey <- getEnvOrThrow "DB_KEY"
  let url = "http://" ++ website ++ "/api/log/clear.php?key=" ++ apiKey
  request <- parseRequest url
  void $ try @SomeException $ httpLbs request man

clearLogsCommand :: Command () ()
clearLogsCommand = Command () CommandInfo
  { commandName = "clearlogs"
  , commandDescription = "" -- TODO
  , commandPerms = AdminLevel
  , commandTimeout = 120
  } $ withArgs $ \() -> do
    clearLogs
    hRespond "Done"