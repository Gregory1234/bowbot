{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.Snipe.Detect where

import BowBot.Discord.Utils
import BowBot.Snipe.Basic
import Data.Char (isDigit)
import BowBot.BotData.Cached

detectDeleteMessage :: (MonadCache SnipeMessage m) => Message -> m ()
detectDeleteMessage m
  | let author = messageAuthor m
  , userAvatar author == Just "a06062af9b11085ab715e340deaab267"
  , userName author == "Dyno"
  , userDiscrim author == Just "0000"
  , userIsWebhook author
  , userIsBot author
  , messageContent m == ""
  , length (messageEmbeds m) == 1 = case fmap unpack . embedDescription $ head (messageEmbeds m) of
    Nothing -> pure ()
    Just embed -> case words (head $ lines embed) of
      ("**Message":"sent":"by":(readMaybe . filter isDigit -> Just sender):"deleted":"in":(readMaybe . filter isDigit -> Just channel):_) -> do
        let content = tail $ dropWhile (/='\n') embed
        void $ storeInCacheIndexed [(channel, SnipeMessage { snipeMessageAuthor = sender, snipeMessageContent = content, snipeMessageWasEdited = False, snipeMessageTimestamp = messageTimestamp m })]
      ("**Message":"edited":"in":(readMaybe . filter isDigit -> Just channel):"[Jump":"to":_) -> do
        case (>>=readMaybe . filter isDigit . unpack . embedFooterText) $ embedFooter $ head (messageEmbeds m) of
          Nothing -> pure ()
          Just sender -> do
            let content = unpack $ embedFieldValue $ head $ embedFields $ head (messageEmbeds m)
            void $ storeInCacheIndexed [(channel, SnipeMessage { snipeMessageAuthor = sender, snipeMessageContent = content, snipeMessageWasEdited = False, snipeMessageTimestamp = messageTimestamp m })]
      _ -> pure ()
detectDeleteMessage _ = pure ()