{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.Perms.Basic where

import BowBot.BotData.Cached
import Discord.Types (UserId)
import BowBot.Discord.Orphans ()
import BowBot.Utils
import BowBot.DB.Basic (queryLog, withDB, executeManyLog', Connection, Only(..))
import Database.MySQL.Simple (Param, Result, ToField(..), FromField(..))
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

getPermissionLevelByDiscord :: (MonadIOReader m r, Has Connection r) => UserId -> m PermissionLevel
getPermissionLevelByDiscord discord = maybe DefaultLevel fromOnly . only <$> queryLog "SELECT `level` FROM `permissions` WHERE `id` = ?" (Only discord)