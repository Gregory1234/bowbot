{-# LANGUAGE TypeFamilies #-}

module BowBot.Snipe.Basic where

import BowBot.Discord.Utils
import BowBot.DB.Typed

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
instance DatabaseTable SnipeMessage where
  type PrimaryKey SnipeMessage = Only ChannelId
  databaseTableName _ = "snipe"
  databaseColumnNames _ = ["author", "content", "edited", "time"]
  databasePrimaryKey _ = ["channel"]

getSnipeMessageByChannel :: (MonadIOReader m r, Has Connection r) => ChannelId -> m (Maybe SnipeMessage)
getSnipeMessageByChannel channel = queryOnlyLogT selectByPrimaryQuery (Only channel)

setSnipeMessageByChannel :: (MonadIOReader m r, Has Connection r) => ChannelId -> SnipeMessage -> m Bool
setSnipeMessageByChannel channel message = (>0) <$> executeLogT insertQueryKeyed (KeyedRow (Only channel) message)