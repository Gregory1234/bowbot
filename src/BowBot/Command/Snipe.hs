{-# LANGUAGE RecordWildCards #-}

module BowBot.Command.Snipe where

import qualified Discord.Requests as R
import BowBot.Command
import Data.Map ((!?))

snipeCommand :: Command
snipeCommand = Command "snipe" DefaultLevel 2 $ do
  channel <- CommandHandler $ \m _ _ -> return $ messageChannel m
  msgs <- hRead snipeMessage
  case msgs !? channel of
    Nothing -> hRespond "*Nothing to snipe!*"
    Just SnipeMessage {..} -> do
      dgid <- hRead discordGuildId
      memoruser <- do
        memebers <- hDiscord $ call $ R.GetGuildMember dgid snipeMessageAuthor
        case memebers of
          Left err1 -> do
            users <- hDiscord $ call $ R.GetUser snipeMessageAuthor
            case users of
              Left err2 -> do
                hLogError $ show err1
                hLogError $ show err2
                return Nothing
              Right u -> return $ Just $ Left u
          Right u -> return $ Just $ Right u
      case memoruser of
        Nothing -> hRespond somethingWrongMessage
        Just mor -> hRespond $ "**" ++ showMemberOrUser True mor ++ "** *wrote:* \n" ++ snipeMessageContent