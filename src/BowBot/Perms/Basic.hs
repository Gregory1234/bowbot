{-# LANGUAGE TypeFamilies #-}

module BowBot.Perms.Basic where

import Discord.Types (UserId)
import BowBot.Discord.Orphans ()
import BowBot.Utils
import BowBot.DB.Typed
import qualified Database.MySQL.Base.Types as T

data PermissionLevel
  = BanLevel
  | DefaultLevel
  | ModLevel
  | AdminLevel
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Param PermissionLevel
instance Result PermissionLevel

instance ToField PermissionLevel where
  toField BanLevel = "ban"
  toField DefaultLevel = "default"
  toField ModLevel = "mod"
  toField AdminLevel = "admin"

instance FromField PermissionLevel where
  fromField = ([T.Enum, T.String], \case
    "ban" -> Right BanLevel
    "default" -> Right DefaultLevel
    "mod" -> Right ModLevel
    "admin" -> Right AdminLevel
    _ -> Left "Wrong permission level")

instance DatabaseTable (Only PermissionLevel) where
  type PrimaryKey (Only PermissionLevel) = UserId
  databaseTableName _ = "permissions"
  databaseColumnNames _ = ["level"]
  databasePrimaryKey _ = "id"

getPermissionLevelByDiscord :: (MonadIOReader m r, Has Connection r) => UserId -> m PermissionLevel
getPermissionLevelByDiscord discord = maybe DefaultLevel fromOnly <$> queryOnlyLogT selectByPrimaryQuery (Only discord)