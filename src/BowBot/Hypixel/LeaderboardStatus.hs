module BowBot.Hypixel.LeaderboardStatus(
  module BowBot.Hypixel.LeaderboardStatus, module BowBot.Minecraft.IsBanned
) where

import BowBot.Utils
import BowBot.DB.Basic
import BowBot.Minecraft.Basic
import BowBot.Minecraft.IsBanned

getHypixelIsBannedByUUID :: (MonadIOReader m r, Has Connection r) => UUID -> m IsBanned
getHypixelIsBannedByUUID uuid = fromMaybe NotBanned <$> queryOnlyLog "SELECT `hypixel` FROM `minecraft` WHERE `uuid` = ?" uuid

setHypixelIsBannedByUUID :: (MonadIOReader m r, Has Connection r) => UUID -> IsBanned -> m Bool
setHypixelIsBannedByUUID uuid banned = (>0) <$> executeLog "UPDATE `minecraft` SET `hypixel` = ? WHERE `uuid` = ?" (banned, uuid)

getHypixelUnbanned :: (MonadIOReader m r, Has Connection r) => m [UUID]
getHypixelUnbanned = queryLog_ "SELECT `uuid` FROM `minecraft` WHERE `hypixel` = 'normal'"