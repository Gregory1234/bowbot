{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.Snipe.Detect where

import BowBot.Discord.Utils
import BowBot.Snipe.Basic
import BowBot.BotData.Cached
import qualified Data.Text as T

detectDeleteMessage :: (MonadIOBotData m d r, HasCache SnipeMessage d) => Message -> m ()
detectDeleteMessage m
  | let author = messageAuthor m
  , userAvatar author == Just "a06062af9b11085ab715e340deaab267"
  , userName author == "Dyno"
  , userDiscrim author == Just "0000"
  , userIsWebhook author
  , userIsBot author
  , messageContent m == ""
  , length (messageEmbeds m) == 1 = case embedDescription $ head (messageEmbeds m) of
    Nothing -> pure ()
    Just embed -> case T.words (head $ T.lines embed) of
      ("**Message":"sent":"by":(readMaybe . unpack . T.filter isDigit -> Just sender):"deleted":"in":(readMaybe . unpack . T.filter isDigit -> Just channel):_) -> do
        let content' = T.dropWhile (/='\n') embed
        --logInfoFork $ "New snipe delete message: " <> pack (show content')
        case T.uncons content' of
          Nothing -> pure () -- TODO: it was probably an image, do something
          Just (_, content) -> 
            void $ storeInCacheIndexed [(channel, SnipeMessage { snipeMessageAuthor = sender, snipeMessageContent = content, snipeMessageWasEdited = False, snipeMessageTimestamp = messageTimestamp m })]
      ("**Message":"edited":"in":(readMaybe . unpack . T.filter isDigit -> Just channel):"[Jump":"to":_) -> do
        case (>>=readMaybe . unpack . T.filter isDigit . embedFooterText) $ embedFooter $ head (messageEmbeds m) of
          Nothing -> pure ()
          Just sender -> do
            let content = embedFieldValue $ head $ embedFields $ head (messageEmbeds m)
            void $ storeInCacheIndexed [(channel, SnipeMessage { snipeMessageAuthor = sender, snipeMessageContent = content, snipeMessageWasEdited = False, snipeMessageTimestamp = messageTimestamp m })]
      _ -> pure ()
detectDeleteMessage _ = pure ()