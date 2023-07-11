{-# LANGUAGE TypeFamilies #-}

module BowBot.Snipe.Table where

import BowBot.Discord.Utils
import BowBot.DB.Typed

data SnipeTable a where
  SnipeTChannel :: SnipeTable ChannelId
  SnipeTAuthor :: SnipeTable UserId
  SnipeTContent :: SnipeTable Text
  SnipeTTime :: SnipeTable UTCTime
  SnipeTEdited :: SnipeTable Bool

instance DatabaseTableLike SnipeTable where
  columnName SnipeTChannel = "channel"
  columnName SnipeTAuthor = "author"
  columnName SnipeTContent = "content"
  columnName SnipeTTime = "time"
  columnName SnipeTEdited = "edited"

instance DatabaseTable SnipeTable where
  newtype PrimaryKey SnipeTable = SnipeTPrimary { getSnipeTPrimary :: ChannelId }
    deriving newtype (QueryParams, QueryResults)
  tableName = "snipe"

instance InTable SnipeTable (PrimaryKey SnipeTable) where
  columnRep = ColRep [SomeCol SnipeTChannel]

type instance MainTable (PrimaryKey SnipeTable) = SnipeTable