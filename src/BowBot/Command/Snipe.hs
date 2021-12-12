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
      dgid <- readProp discordGuildId bdt
      memoruser <- do
        memebers <- call $ R.GetGuildMember dgid snipeMessageAuthor
        case memebers of
          Left err1 -> do
            users <- call $ R.GetUser snipeMessageAuthor
            case users of
              Left err2 -> do
                logError man $ show err1
                logError man $ show err2
                return Nothing
              Right u -> return $ Just $ Left u
          Right u -> return $ Just $ Right u
      case memoruser of
        Nothing -> respond m somethingWrongMessage
        Just mor -> respond m $ "**" ++ showMemberOrUser True mor ++ "** *wrote:* \n" ++ snipeMessageContent