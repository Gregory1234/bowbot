{-# LANGUAGE TypeFamilies #-}

module BowBot.Hypixel.Stats.Table where

import Data.Time
import BowBot.Minecraft.Basic
import BowBot.DB.Typed

data StatsTable a where
  StatsTUUID :: StatsTable UUID
  StatsTWins :: StatsTable Integer
  StatsTLosses :: StatsTable Integer
  StatsTLastUpdate :: StatsTable (Maybe UTCTime)
  StatsTWinstreak :: StatsTable (Maybe Integer)
  StatsTLastWinstreakUpdate :: StatsTable (Maybe UTCTime)
  StatsTAnnouncementWins :: StatsTable (Maybe Integer)

instance DatabaseTableLike StatsTable where
  columnName StatsTUUID = "minecraft_uuid"
  columnName StatsTWins = "wins"
  columnName StatsTLosses = "losses"
  columnName StatsTLastUpdate = "last_update"
  columnName StatsTWinstreak = "winstreak"
  columnName StatsTLastWinstreakUpdate = "last_winstreak_update"
  columnName StatsTAnnouncementWins = "announcement_wins"

instance DatabaseTable StatsTable where
  newtype PrimaryKey StatsTable = StatsTPrimary { getStatsTPrimary :: UUID }
    deriving newtype (QueryParams, QueryResults)
  tableName = "hypixel_bow_stats"

instance InTable StatsTable (PrimaryKey StatsTable) where
  columnRep = ColRep [SomeCol StatsTUUID]

type instance MainTable (PrimaryKey StatsTable) = StatsTable