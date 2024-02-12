{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module BowBot.Ranked.Report where

import BowBot.Account.Basic
import BowBot.DB.Basic
import BowBot.Discord.Utils
import qualified Data.Text as T
import Language.MySQL.Query

data RankedBowScore = RankedBowScore { rankedScore1 :: Integer, rankedScore2 :: Integer }
  deriving (Show, Eq, Generic)
  deriving (ToMysql, FromMysql) via (Generically RankedBowScore)

rankedBowScoreFromString :: Bool -> Text -> Maybe RankedBowScore
rankedBowScoreFromString first (T.splitOn "-" -> [readMaybe . unpack -> Just a, readMaybe . unpack -> Just b])
  | 0 <= a, 0 <= b, a <= 3, b <= 3, (a == 3) /= (b == 3) = Just $ if first then RankedBowScore a b else RankedBowScore b a
rankedBowScoreFromString _ _ = Nothing

data RankedBowReportStatus = ReportActive | ReportRejected
  deriving (Show, Eq, Enum)
  deriving (ToMysqlSimple, FromMysqlSimple, ToMysql, FromMysql) via (EnumValue RankedBowReportStatus)

instance MysqlString RankedBowReportStatus

instance MysqlEnum RankedBowReportStatus where
  toMysqlEnum ReportActive = "active"
  toMysqlEnum ReportRejected = "rejected"
  fromMysqlEnum "active" = ReportActive
  fromMysqlEnum "rejected" = ReportRejected
  fromMysqlEnum _ = error "Wrong report status"

data RankedBowReport = RankedBowReport
  { reportAuthor :: BowBotId
  , reportGameId :: Integer
  , reportMessage :: MessageId
  , reportScore :: RankedBowScore
  , reportStatus :: RankedBowReportStatus
  } deriving (Show, Eq, Generic)
    deriving (ToMysql, FromMysql) via (Generically RankedBowReport)

$(pure [])

getRankedBowReportByAuthor :: (MonadIOReader m r, Has SafeMysqlConn r) => BowBotId -> m (Maybe RankedBowReport)
getRankedBowReportByAuthor bid = queryOnlyLog [mysql|SELECT RankedBowReport FROM `ranked_bow_report` WHERE `author` = bid|]

getRankedBowReportByMessageId :: (MonadIOReader m r, Has SafeMysqlConn r) => MessageId -> m (Maybe RankedBowReport)
getRankedBowReportByMessageId mid = queryOnlyLog [mysql|SELECT RankedBowReport FROM `ranked_bow_report` WHERE `message_id` = mid|]

createRankedBowReport :: (MonadIOReader m r, Has SafeMysqlConn r) => BowBotId -> Integer -> MessageId -> RankedBowScore -> m Bool
createRankedBowReport bid gid mid score = (>0) <$> executeLog [mysql|INSERT INTO `ranked_bow_report`(`author`, ^`game_id`, ^`message_id`, RankedBowScore, ^`status`) VALUES (bid, gid, mid, score, "active")|]

setRankedBowReportStatusByMessageId :: (MonadIOReader m r, Has SafeMysqlConn r) => MessageId -> RankedBowReportStatus -> m Bool
setRankedBowReportStatusByMessageId mid status = (>0) <$> executeLog [mysql|UPDATE `ranked_bow_report` SET `status` = status WHERE `message_id` = mid|]
