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

instance ToMysql SnipeMessage where
  toActions SnipeMessage {..} = toActions snipeMessageAuthor ++ toActions snipeMessageContent ++ toActions snipeMessageWasEdited ++ toActions snipeMessageTimestamp
instance FromMysql SnipeMessage where
  rowParser = SnipeMessage <$> rowParser <*> rowParser <*> rowParser <*> rowParser
instance DatabaseTable SnipeMessage where
  type PrimaryKey SnipeMessage = ChannelId
  databaseTableName _ = "snipe"
  databaseColumnNames _ = ["author", "content", "edited", "time"]
  databasePrimaryKey _ = ["channel"]

getSnipeMessageByChannel :: (MonadIOReader m r, Has Connection r) => ChannelId -> m (Maybe SnipeMessage)
getSnipeMessageByChannel channel = queryOnlyLogT selectByPrimaryQuery channel

setSnipeMessageByChannel :: (MonadIOReader m r, Has Connection r) => ChannelId -> SnipeMessage -> m Bool
setSnipeMessageByChannel channel message = (>0) <$> executeLogT insertQueryKeyed (KeyedRow channel message)