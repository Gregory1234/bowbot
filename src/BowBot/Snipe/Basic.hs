{-# LANGUAGE TypeFamilies #-}

module BowBot.Snipe.Basic where

import BowBot.Discord.Utils
import BowBot.DB.Typed
import BowBot.Snipe.Table

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
instance InTable SnipeTable SnipeMessage where
  columnRep = ColRep [SomeCol SnipeTAuthor, SomeCol SnipeTContent, SomeCol SnipeTEdited, SomeCol SnipeTTime]

getSnipeMessageByChannel :: (MonadIOReader m r, Has Connection r) => ChannelId -> m (Maybe SnipeMessage)
getSnipeMessageByChannel cid = queryOnlyLogT selectByPrimaryQuery (SnipeTPrimary cid)

setSnipeMessageByChannel :: (MonadIOReader m r, Has Connection r) => ChannelId -> SnipeMessage -> m Bool
setSnipeMessageByChannel channel message = (>0) <$> executeLogT insertQuery' (SnipeTPrimary channel, message)