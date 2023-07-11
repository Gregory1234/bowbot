{-# LANGUAGE TypeFamilies #-}

module BowBot.Hypixel.TimeStats.Table where

import Data.Time
import BowBot.Minecraft.Basic
import BowBot.DB.Typed
import BowBot.Stats.TimeRange

data TimeStatsTable a where
  TimeStatsTUUID :: TimeStatsTable UUID
  TimeStatsTWins :: TimeStatsTable Integer
  TimeStatsTLosses :: TimeStatsTable Integer
  TimeStatsTLastUpdate :: TimeStatsTable (Maybe UTCTime)
  TimeStatsTTime :: TimeStatsTable TimeRange

instance DatabaseTableLike TimeStatsTable where
  columnName TimeStatsTUUID = "minecraft_uuid"
  columnName TimeStatsTWins = "wins"
  columnName TimeStatsTLosses = "losses"
  columnName TimeStatsTLastUpdate = "last_update"
  columnName TimeStatsTTime = "time"

instance DatabaseTable TimeStatsTable where
  data PrimaryKey TimeStatsTable = TimeStatsTPrimary { getTimeStatsTPrimaryUUID :: UUID, getTimeStatsTPrimaryTime :: TimeRange }
  tableName = "hypixel_bow_timed_stats"

instance QueryParams (PrimaryKey TimeStatsTable) where
  renderParams TimeStatsTPrimary {..} = [render getTimeStatsTPrimaryUUID, render getTimeStatsTPrimaryTime]

instance QueryResults (PrimaryKey TimeStatsTable) where
  convertResults = TimeStatsTPrimary <$> convertResults <*> convertResults

instance InTable TimeStatsTable (PrimaryKey TimeStatsTable) where
  columnRep = ColRep [SomeCol TimeStatsTUUID]

type instance MainTable (PrimaryKey TimeStatsTable) = TimeStatsTable