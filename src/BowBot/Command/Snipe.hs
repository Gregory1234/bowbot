module BowBot.Command.Snipe where

import qualified Discord.Requests as R
import BowBot.Command
import Data.Map ((!?))

snipeCommand :: Command
snipeCommand = Command "snipe" DefaultLevel 2 $ \m _ bdt -> do
  msgs <- liftIO $ atomically $ readTVar (snipeMessage bdt)
  case msgs !? messageChannel m of
    Nothing -> respond m "*Nothing to snipe!*"
    Just (author, msg) -> do
      users <- restCall $ R.GetUser author
      case users of
        Left err -> do
          liftIO $ print err
          respond m somethingWrongMessage
        Right u -> do
          respond m $ "**" ++ unpack (userName u) ++ "#" ++ unpack (userDiscrim u) ++ "** *wrote:* \n" ++ msg