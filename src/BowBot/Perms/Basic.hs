{-# LANGUAGE QuasiQuotes #-}

module BowBot.Perms.Basic where

import Discord.Types (UserId)
import BowBot.Discord.Orphans ()
import BowBot.Utils
import BowBot.DB.Basic

data PermissionLevel
  = BanLevel
  | DefaultLevel
  | ModLevel
  | AdminLevel
  deriving (Eq, Ord, Enum, Bounded, Show)
  deriving (ToMysqlSimple, FromMysqlSimple, ToMysql, FromMysql) via (EnumValue PermissionLevel)

instance MysqlEnum PermissionLevel where
  toMysqlEnum BanLevel = "ban"
  toMysqlEnum DefaultLevel = "default"
  toMysqlEnum ModLevel = "mod"
  toMysqlEnum AdminLevel = "admin"
  fromMysqlEnum "ban" = BanLevel
  fromMysqlEnum "default" = DefaultLevel
  fromMysqlEnum "mod" = ModLevel
  fromMysqlEnum "admin" = AdminLevel
  fromMysqlEnum _ = error "Wrong permission level"

getPermissionLevelByDiscord :: (MonadIOReader m r, Has SafeMysqlConn r) => UserId -> m PermissionLevel
getPermissionLevelByDiscord discord = fromMaybe DefaultLevel <$> queryOnlyLog [mysql|SELECT `level` FROM `permissions` WHERE `discord_id` = discord|]