{-# LANGUAGE QuasiQuotes #-}

module BowBot.Hypixel.LeaderboardStatus where

import BowBot.Utils
import BowBot.DB.Basic
import Language.MySQL.Query
import BowBot.Minecraft.Basic

data IsBanned
  = NotBanned
  | Banned
  deriving (Eq, Ord, Enum, Bounded, Show)
  deriving (ToMysqlSimple, FromMysqlSimple, ToMysql, FromMysql) via (EnumValue IsBanned)

instance MysqlString IsBanned -- TODO: add static correctness checking

instance MysqlEnum IsBanned where
  toMysqlEnum NotBanned = "normal"
  toMysqlEnum Banned = "ban"
  fromMysqlEnum "normal" = NotBanned
  fromMysqlEnum "ban" = Banned
  fromMysqlEnum _ = error "Wrong ban status"

getHypixelIsBannedByUUID :: (MonadIOReader m r, Has SafeMysqlConn r) => UUID -> m IsBanned
getHypixelIsBannedByUUID uuid = fromMaybe NotBanned <$> queryOnlyLog [mysql|SELECT `hypixel` FROM `minecraft` WHERE `uuid` = uuid|]

setHypixelIsBannedByUUID :: (MonadIOReader m r, Has SafeMysqlConn r) => UUID -> IsBanned -> m Bool
setHypixelIsBannedByUUID uuid banned = (>0) <$> executeLog [mysql|UPDATE `minecraft` SET `hypixel` = banned WHERE `uuid` = uuid|]

getHypixelUnbanned :: (MonadIOReader m r, Has SafeMysqlConn r) => m [UUID]
getHypixelUnbanned = queryLog [mysql|SELECT `uuid` FROM `minecraft` WHERE `hypixel` = 'normal'|]