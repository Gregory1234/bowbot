{-# LANGUAGE TypeFamilies #-}

module BowBot.Snipe.Basic where

import BowBot.BotData.Cached
import BowBot.Discord.Utils

data SnipeMessage = SnipeMessage
  { snipeMessageAuthor :: UserId
  , snipeMessageContent :: String
  , snipeMessageWasEdited :: Bool
  , snipeMessageTimestamp :: UTCTime
  } deriving Show

instance Cached SnipeMessage where -- TODO: not really...
  type CacheIndex SnipeMessage = ChannelId
  refreshCache _ = pure ()

instance CachedStorable SnipeMessage where
  storeInCacheIndexed snipes = do
    cache <- getCache
    liftIO $ atomically $ modifyTVar cache $ insertMany snipes
    return True