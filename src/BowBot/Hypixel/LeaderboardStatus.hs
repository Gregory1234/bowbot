{-# LANGUAGE QuasiQuotes #-}

module BowBot.Hypixel.LeaderboardStatus where

import BowBot.Utils
import BowBot.DB.Typed
import Language.MySQL.Query
import qualified Database.MySQL.Base.Types as T
import BowBot.Minecraft.Basic

data IsBanned
  = NotBanned
  | Banned
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Param IsBanned
instance Result IsBanned

deriving via (SimpleValue IsBanned) instance ToMysql IsBanned
deriving via (SimpleValue IsBanned) instance FromMysql IsBanned

instance MysqlString IsBanned -- TODO: add static correctness checking

instance ToField IsBanned where
  toField NotBanned = "normal"
  toField Banned = "ban"

instance FromField IsBanned where
  fromField = ([T.Enum, T.String], \case
    "normal" -> Right NotBanned
    "ban" -> Right Banned
    _ -> Left "Wrong ban status")

getHypixelIsBannedByUUID :: (MonadIOReader m r, Has Connection r) => UUID -> m IsBanned
getHypixelIsBannedByUUID uuid = fromMaybe NotBanned <$> queryOnlyLogT [mysql|SELECT `hypixel` FROM `minecraft` WHERE `uuid` = uuid|]

setHypixelIsBannedByUUID :: (MonadIOReader m r, Has Connection r) => UUID -> IsBanned -> m Bool
setHypixelIsBannedByUUID uuid banned = (>0) <$> executeLog "UPDATE `minecraft` SET `hypixel` = ? WHERE `uuid` = ?" (banned, uuid)

getHypixelUnbanned :: (MonadIOReader m r, Has Connection r) => m [UUID]
getHypixelUnbanned = queryLogT [mysql|SELECT `uuid` FROM `minecraft` WHERE `hypixel` = 'normal'|]