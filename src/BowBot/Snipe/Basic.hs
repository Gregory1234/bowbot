{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module BowBot.Snipe.Basic where

import BowBot.Discord.Utils
import BowBot.DB.Basic

data SnipeMessage = SnipeMessage
  { snipeMessageAuthor :: !UserId
  , snipeMessageContent :: !Text
  , snipeMessageWasEdited :: !Bool
  , snipeMessageTimestamp :: !UTCTime
  } deriving stock (Show, Eq, Generic)
    deriving (ToMysql, FromMysql) via (Generically SnipeMessage)

$(pure [])

getSnipeMessageByChannel :: (MonadIOReader m r, Has Connection r) => ChannelId -> m (Maybe SnipeMessage)
getSnipeMessageByChannel channel = queryOnlyLog [mysql|SELECT SnipeMessage FROM `snipe` WHERE `channel` = channel|]

setSnipeMessageByChannel :: (MonadIOReader m r, Has Connection r) => ChannelId -> SnipeMessage -> m Bool
setSnipeMessageByChannel channel message = (>0) <$> executeLog [mysql|INSERT INTO `snipe`(`channel`, SnipeMessage) VALUES (channel, message)|]