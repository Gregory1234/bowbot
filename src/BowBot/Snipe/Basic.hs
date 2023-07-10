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
  renderParams SnipeMessage {..} = renderParams snipeMessageAuthor ++ renderParams snipeMessageContent ++ renderParams snipeMessageWasEdited ++ renderParams snipeMessageTimestamp
instance QueryResults SnipeMessage where
  convertResults = SnipeMessage <$> convert <*> convert <*> convert <*> convert
instance DatabaseTable SnipeMessage where
  type PrimaryKey SnipeMessage = ChannelId
  databaseTableName _ = "snipe"
  databaseColumnNames _ = ["author", "content", "edited", "time"]
  databasePrimaryKey _ = ["channel"]

getSnipeMessageByChannel :: (MonadIOReader m r, Has Connection r) => ChannelId -> m (Maybe SnipeMessage)
getSnipeMessageByChannel = queryOnlyLogT selectByPrimaryQuery

setSnipeMessageByChannel :: (MonadIOReader m r, Has Connection r) => ChannelId -> SnipeMessage -> m Bool
setSnipeMessageByChannel channel message = (>0) <$> executeLogT insertQueryKeyed (KeyedRow channel message)