module BowBot.Perms.Type where

import BowBot.DB.Typed
import qualified Database.MySQL.Base.Types as T

data PermissionLevel
  = BanLevel
  | DefaultLevel
  | ModLevel
  | AdminLevel
  deriving (Eq, Ord, Enum, Bounded, Show)
  deriving (QueryParams, QueryResults) via (SimpleValue PermissionLevel)

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