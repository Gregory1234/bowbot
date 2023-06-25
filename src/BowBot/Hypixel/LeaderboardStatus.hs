module BowBot.Hypixel.LeaderboardStatus where

import BowBot.Utils
import BowBot.DB.Basic
import qualified Database.MySQL.Base.Types as T
import BowBot.Minecraft.Basic

data IsBanned
  = NotBanned
  | Banned
  deriving (Eq, Ord, Enum, Bounded, Show)

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

getHypixelIsBannedByUUID :: (MonadIOReader m r, Has Connection r) => UUID -> m IsBanned
getHypixelIsBannedByUUID uuid = maybe NotBanned fromOnly . only <$> queryLog "SELECT `hypixel` FROM `minecraft` WHERE `uuid` = ?" (Only uuid)

setHypixelIsBannedByUUID :: (MonadIOReader m r, Has Connection r) => UUID -> IsBanned -> m Bool
setHypixelIsBannedByUUID uuid banned = (>0) <$> executeLog "UPDATE `minecraft` SET `hypixel` = ? WHERE `uuid` = ?" (banned, uuid)

getHypixelUnbanned :: (MonadIOReader m r, Has Connection r) => m [UUID]
getHypixelUnbanned = map fromOnly <$> queryLog "SELECT `uuid` FROM `minecraft` WHERE `hypixel` = 'normal'" ()