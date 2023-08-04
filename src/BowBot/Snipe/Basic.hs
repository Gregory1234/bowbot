{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module BowBot.Snipe.Basic where

import BowBot.Discord.Utils
import BowBot.DB.Typed

data SnipeMessage = SnipeMessage
  { snipeMessageAuthor :: !UserId
  , snipeMessageContent :: !Text
  , snipeMessageWasEdited :: !Bool
  , snipeMessageTimestamp :: !UTCTime
  } deriving Show

instance ToMysql SnipeMessage where
  toActions SnipeMessage {..} = toActions snipeMessageAuthor ++ toActions snipeMessageContent ++ toActions snipeMessageWasEdited ++ toActions snipeMessageTimestamp
instance FromMysql SnipeMessage where
  rowParser = SnipeMessage <$> rowParser <*> rowParser <*> rowParser <*> rowParser

$(pure [])

getSnipeMessageByChannel :: (MonadIOReader m r, Has Connection r) => ChannelId -> m (Maybe SnipeMessage)
getSnipeMessageByChannel channel = queryOnlyLogT [mysql|SELECT SnipeMessage FROM `snipe` WHERE `channel` = channel|]

setSnipeMessageByChannel :: (MonadIOReader m r, Has Connection r) => ChannelId -> SnipeMessage -> m Bool
setSnipeMessageByChannel channel message = (>0) <$> executeLogT [mysql|INSERT INTO `snipe`(`channel`, SnipeMessage) VALUES (channel, message)|]