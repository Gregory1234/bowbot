{-# LANGUAGE TypeFamilies #-}

module BowBot.Hypixel.Table where

import Data.Time
import BowBot.Minecraft.Basic
import BowBot.DB.Typed
import BowBot.Hypixel.StatsTimeRange

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

data TimeStatsTable a where
  TimeStatsTUUID :: TimeStatsTable UUID
  TimeStatsTWins :: TimeStatsTable Integer
  TimeStatsTLosses :: TimeStatsTable Integer
  TimeStatsTLastUpdate :: TimeStatsTable (Maybe UTCTime)
  TimeStatsTTime :: TimeStatsTable StatsTimeRange

instance DatabaseTableLike TimeStatsTable where
  columnName TimeStatsTUUID = "minecraft_uuid"
  columnName TimeStatsTWins = "wins"
  columnName TimeStatsTLosses = "losses"
  columnName TimeStatsTLastUpdate = "last_update"
  columnName TimeStatsTTime = "time"

instance DatabaseTable TimeStatsTable where
  data PrimaryKey TimeStatsTable = TimeStatsTPrimary { getTimeStatsTPrimaryUUID :: UUID, getTimeStatsTPrimaryTime :: StatsTimeRange }
  tableName = "hypixel_bow_timed_stats"

instance QueryParams (PrimaryKey TimeStatsTable) where
  renderParams TimeStatsTPrimary {..} = [render getTimeStatsTPrimaryUUID, render getTimeStatsTPrimaryTime]

instance QueryResults (PrimaryKey TimeStatsTable) where
  convertResults = TimeStatsTPrimary <$> convertResults <*> convertResults

instance InTable TimeStatsTable (PrimaryKey TimeStatsTable) where
  columnRep = ColRep [SomeCol TimeStatsTUUID]

type instance MainTable (PrimaryKey TimeStatsTable) = TimeStatsTable