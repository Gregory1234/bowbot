{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module BowBot.Ranked.Game where

import BowBot.Discord.Basic
import BowBot.Account.Basic
import BowBot.DB.Basic
import BowBot.Discord.Utils

data RankedGameStatus = GameActive | GameCompleted | GameAbandoned
  deriving (Show, Eq, Enum)
  deriving (ToMysqlSimple, FromMysqlSimple, ToMysql, FromMysql) via (EnumValue RankedGameStatus)

instance MysqlEnum RankedGameStatus where
  toMysqlEnum GameActive = "active"
  toMysqlEnum GameCompleted = "completed"
  toMysqlEnum GameAbandoned = "abandoned"
  fromMysqlEnum "active" = GameActive
  fromMysqlEnum "completed" = GameCompleted
  fromMysqlEnum "abandoned" = GameAbandoned
  fromMysqlEnum _ = error "Wrong game status"

data RankedBowGame = RankedBowGame
  { rankedGameId :: Integer
  , rankedGameTime :: UTCTime
  , rankedPlayers :: (BowBotId, BowBotId)
  , rankedGameStatus :: RankedGameStatus
  } deriving (Show, Generic)
    deriving (ToMysql, FromMysql) via (Generically RankedBowGame)

$(pure [])

createRankedGame :: (MonadIOReader m r, Has SafeMysqlConn r) => (BowBotId, BowBotId) -> m (Maybe Integer)
createRankedGame players@(p1, p2) = do
  ctx <- ask
  liftIO $ flip runReaderT ctx $ withTransaction $ do
    ret <- executeIDLog [mysql|INSERT AI INTO `ranked_bow_game`(`player1`, `player2`) VALUES (players)|]
    if isNothing ret
      then rollback $> Nothing
      else do
        c <- executeLog [mysql|UPDATE `ranked_bow_stats` SET `current_game` = ret WHERE `account_id` IN (p1, p2) AND `current_game` = NULL|]
        if c == 2 then return ret else rollback $> Nothing

getRankedGame :: (MonadIOReader m r, Has SafeMysqlConn r) => Integer -> m (Maybe RankedBowGame)
getRankedGame gid = queryOnlyLog [mysql|SELECT RankedBowGame FROM `ranked_bow_game` WHERE `id` = gid|]