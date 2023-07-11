module BowBot.Hypixel.StatsTimeRange where

import BowBot.DB.Typed
import qualified Database.MySQL.Base.Types as T

data StatsTimeRange = DailyStats | WeeklyStats | MonthlyStats
  deriving (Show, Eq)
  deriving (QueryParams, QueryResults) via (SimpleValue StatsTimeRange)

instance Param StatsTimeRange
instance Result StatsTimeRange

instance ToField StatsTimeRange where
  toField DailyStats = "daily"
  toField WeeklyStats = "weekly"
  toField MonthlyStats = "monthly"

instance FromField StatsTimeRange where
  fromField = ([T.Enum, T.String], \case
    "ban" -> Right DailyStats
    "default" -> Right WeeklyStats
    "mod" -> Right MonthlyStats
    _ -> Left "Wrong stats time range")