{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module BowBot.Snipe where

import BowBot.Command
import Control.Concurrent.STM (STM)
import Data.Char (isDigit)
import Data.Map (insert)


detectDeleteMessage :: BotData -> Message -> STM ()
detectDeleteMessage bdt m
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
        modifyTVar (snipeMessage bdt) $ insert channel (sender, content)
      _ -> pure ()
detectDeleteMessage _ _ = pure ()