{-# LANGUAGE QuasiQuotes #-}

module BowBot.Perms.Basic where

import Discord.Types (UserId)
import BowBot.Discord.Orphans ()
import BowBot.Utils
import BowBot.DB.Typed

data PermissionLevel
  = BanLevel
  | DefaultLevel
  | ModLevel
  | AdminLevel
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Param PermissionLevel
instance Result PermissionLevel

deriving via (SimpleValue PermissionLevel) instance ToMysql PermissionLevel
deriving via (SimpleValue PermissionLevel) instance FromMysql PermissionLevel

instance ToField PermissionLevel where
  toField BanLevel = "ban"
  toField DefaultLevel = "default"
  toField ModLevel = "mod"
  toField AdminLevel = "admin"

instance FromField PermissionLevel where
  fromField = (textSqlTypes, \case
    "ban" -> Right BanLevel
    "default" -> Right DefaultLevel
    "mod" -> Right ModLevel
    "admin" -> Right AdminLevel
    _ -> Left "Wrong permission level")

getPermissionLevelByDiscord :: (MonadIOReader m r, Has Connection r) => UserId -> m PermissionLevel
getPermissionLevelByDiscord discord = fromMaybe DefaultLevel <$> queryOnlyLogT [mysql|SELECT `level` FROM `permissions` WHERE `discord_id` = discord|]