{-# LANGUAGE TypeFamilies #-}

module BowBot.Snipe.Basic where

import BowBot.Discord.Utils
import BowBot.DB.Basic

data SnipeMessage = SnipeMessage
  { snipeMessageAuthor :: !UserId
  , snipeMessageContent :: !Text
  , snipeMessageWasEdited :: !Bool
  , snipeMessageTimestamp :: !UTCTime
  } deriving Show

instance QueryParams SnipeMessage where
  renderParams SnipeMessage {..} = renderParams (snipeMessageAuthor, snipeMessageContent, snipeMessageWasEdited, snipeMessageTimestamp)
instance QueryResults SnipeMessage where
  convertResults fields strings = let
    (snipeMessageAuthor, snipeMessageContent, snipeMessageWasEdited, snipeMessageTimestamp) = convertResults fields strings
      in SnipeMessage {..}
instance QueryResultsSize SnipeMessage where
  queryResultsSize _ = 4

getSnipeMessageByChannel :: (MonadIOReader m r, Has Connection r) => ChannelId -> m (Maybe SnipeMessage)
getSnipeMessageByChannel channel = only <$> queryLog "SELECT `author`, `content`, `edited`, `time` FROM `snipe` WHERE `channel` = ?" (Only channel)

setSnipeMessageByChannel :: (MonadIOReader m r, Has Connection r) => ChannelId -> SnipeMessage -> m Bool
setSnipeMessageByChannel channel message = (>0) <$> executeLog "INSERT INTO `snipe` (`channel`, `author`, `content`, `edited`, `time`) VALUES (?,?,?,?,?) ON DUPLICATE KEY UPDATE `author`=VALUES(`author`), `content`=VALUES(`content`), `edited`=VALUES(`edited`), `time`=VALUES(`time`)" (Concat (Only channel, message))