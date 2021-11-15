{-# LANGUAGE RecordWildCards #-}

module BowBot.Command.Snipe where

import qualified Discord.Requests as R
import BowBot.Command
import Data.Map ((!?))
import BowBot.API

snipeCommand :: Command
snipeCommand = Command "snipe" DefaultLevel 2 $ \m man bdt -> do
  msgs <- readProp snipeMessage bdt
  case msgs !? messageChannel m of
    Nothing -> respond m "*Nothing to snipe!*"
    Just SnipeMessage {..} -> do
      users <- call $ R.GetUser snipeMessageAuthor
      case users of
        Left err -> do
          logError man $ show err
          respond m somethingWrongMessage
        Right u -> do
          respond m $ "**" ++ unpack (userName u) ++ "#" ++ unpack (userDiscrim u) ++ "** *wrote:* \n" ++ snipeMessageContent