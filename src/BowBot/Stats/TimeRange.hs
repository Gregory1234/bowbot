module BowBot.Stats.TimeRange where

import BowBot.DB.Typed
import qualified Database.MySQL.Base.Types as T
import Data.Text (Text)

data TimeRange = DailyStats | WeeklyStats | MonthlyStats
  deriving (Show, Eq)
  deriving (QueryParams, QueryResults) via (SimpleValue TimeRange)

instance Param TimeRange
instance Result TimeRange

instance ToField TimeRange where
  toField DailyStats = "daily"
  toField WeeklyStats = "weekly"
  toField MonthlyStats = "monthly"

instance FromField TimeRange where
  fromField = ([T.Enum, T.String], \case
    "ban" -> Right DailyStats
    "default" -> Right WeeklyStats
    "mod" -> Right MonthlyStats
    _ -> Left "Wrong stats time range")

timeRangeName :: TimeRange -> Text
timeRangeName DailyStats = "Day"
timeRangeName WeeklyStats = "Week"
timeRangeName MonthlyStats = "Month"