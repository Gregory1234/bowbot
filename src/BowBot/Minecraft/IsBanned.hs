module BowBot.Minecraft.IsBanned where

import BowBot.DB.Typed
import qualified Database.MySQL.Base.Types as T

data IsBanned
  = NotBanned
  | Banned
  deriving (Eq, Ord, Enum, Bounded, Show)
  deriving (QueryParams, QueryResults) via (SimpleValue IsBanned)

instance Param IsBanned
instance Result IsBanned

instance ToField IsBanned where
  toField NotBanned = "normal"
  toField Banned = "ban"

instance FromField IsBanned where
  fromField = ([T.Enum, T.String], \case
    "normal" -> Right NotBanned
    "ban" -> Right Banned
    _ -> Left "Wrong ban status")
