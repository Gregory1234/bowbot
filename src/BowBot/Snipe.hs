{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module BowBot.Snipe where

import BowBot.Command
import Data.Char (isDigit)
import Data.Map (insert)

detectDeleteMessage :: (BotDataMonad m) => Message -> m ()
detectDeleteMessage m
  | let author = messageAuthor m
  , userAvatar author == Just "a06062af9b11085ab715e340deaab267"
  , userName author == "Dyno"
  , userDiscrim author == "0000"
  , userIsWebhook author
  , userIsBot author
  , messageText m == "" 
  , length (messageEmbeds m) == 1 = case fmap unpack . embedDescription $ head (messageEmbeds m) of
    Nothing -> pure ()
    Just embed -> case words (head $ lines embed) of
      ("**Message":"sent":"by":(readMaybe . filter isDigit -> Just sender):"deleted":"in":(readMaybe . filter isDigit -> Just channel):_) -> do
        let content = tail $ dropWhile (/='\n') embed
        hModify snipeMessage $ insert channel
          SnipeMessage { snipeMessageAuthor = sender, snipeMessageContent = content, snipeMessageWasEdited = False, snipeMessageTimestamp = messageTimestamp m }
      ("**Message":"edited":"in":(readMaybe . filter isDigit -> Just channel):"[Jump":"to":_) -> do
        case (>>=readMaybe . filter isDigit . unpack . embedFooterText) $ embedFooter $ head (messageEmbeds m) of
          Nothing -> pure ()
          Just sender -> do
            let content = unpack $ embedFieldValue $ head $ embedFields $ head (messageEmbeds m)
            hModify snipeMessage $ insert channel 
              SnipeMessage { snipeMessageAuthor = sender, snipeMessageContent = content, snipeMessageWasEdited = False, snipeMessageTimestamp = messageTimestamp m }
      _ -> pure ()
detectDeleteMessage _ = pure ()