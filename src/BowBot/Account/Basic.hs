{-# LANGUAGE TypeFamilies #-}

module BowBot.Account.Basic where

import BowBot.Minecraft.Basic (UUID(..))
import Discord.Internal.Rest (UserId)
import BowBot.DB.Basic (queryLog, Only(..), Connection)
import BowBot.Utils
import BowBot.Discord.Orphans ()
import Data.Hashable (Hashable)
import Database.MySQL.Simple (Param, Result)

newtype BowBotId = BowBotId { unBowBotId :: Integer }
  deriving stock (Show, Eq, Ord)
  deriving newtype (Hashable, Param, Result)

data MinecraftList = MinecraftList { selectedMinecraft :: UUID, allMinecrafts :: [UUID] }

minecraftListFromList :: [(UUID, Bool)] -> Maybe MinecraftList
minecraftListFromList mcs = fmap (\selectedMinecraft -> MinecraftList {..}) selectedMinecraft'
  where
    selectedMinecraft' = fmap fst $ only $ filter snd mcs
    allMinecrafts = map fst mcs

getSelectedMinecraftUUIDByDiscord :: (MonadIOReader m r, Has Connection r) => UserId -> m (Maybe UUID)
getSelectedMinecraftUUIDByDiscord did = only . map fromOnly <$> queryLog "SELECT `minecraft` FROM `peopleMinecraft` JOIN `peopleDiscord` ON `peopleMinecraft`.`id`=`peopleDiscord`.`id` WHERE `peopleDiscord`.`discord` = ? AND `peopleMinecraft`.`selected` = 1" (Only did)

getMinecraftUUIDsByDiscord :: (MonadIOReader m r, Has Connection r) => UserId -> m [UUID]
getMinecraftUUIDsByDiscord did = map fromOnly <$> queryLog "SELECT `minecraft` FROM `peopleMinecraft` JOIN `peopleDiscord` ON `peopleMinecraft`.`id`=`peopleDiscord`.`id` WHERE `peopleDiscord`.`discord` = ?" (Only did)

getMinecraftListByDiscord :: (MonadIOReader m r, Has Connection r) => UserId -> m (Maybe MinecraftList)
getMinecraftListByDiscord did = minecraftListFromList <$> queryLog "SELECT `minecraft`,`selected` FROM `peopleMinecraft` JOIN `peopleDiscord` ON `peopleMinecraft`.`id`=`peopleDiscord`.`id` WHERE `peopleDiscord`.`discord` = ?" (Only did)

getMinecraftListByBowBotId :: (MonadIOReader m r, Has Connection r) => BowBotId -> m (Maybe MinecraftList)
getMinecraftListByBowBotId bid = minecraftListFromList <$> queryLog "SELECT `minecraft`,`selected` FROM `peopleMinecraft` WHERE `id` = ?" (Only bid)

getDiscordIdsByMinecraft :: (MonadIOReader m r, Has Connection r) => UUID -> m [UserId]
getDiscordIdsByMinecraft uuid = map fromOnly <$> queryLog "SELECT `discord` FROM `peopleDiscord` JOIN `peopleMinecraft` ON `peopleMinecraft`.`id`=`peopleDiscord`.`id` WHERE `peopleMinecraft`.`minecraft` = ?" (Only uuid)

getDiscordIdsByBowBotId :: (MonadIOReader m r, Has Connection r) => BowBotId -> m [UserId]
getDiscordIdsByBowBotId bid = map fromOnly <$> queryLog "SELECT `discord` FROM `peopleDiscord` WHERE `id` = ?" (Only bid)

getDiscordIdsByDiscord :: (MonadIOReader m r, Has Connection r) => UserId -> m [UserId]
getDiscordIdsByDiscord uuid = map fromOnly <$> queryLog "SELECT `peopleDiscord2`.`discord` FROM `peopleDiscord` JOIN `peopleDiscord` AS `peopleDiscord2` ON `peopleDiscord`.`id` = `peopleDiscord2`.`id` WHERE `peopleDiscord`.`discord` = ?" (Only uuid)

getBowBotIdByDiscord :: (MonadIOReader m r, Has Connection r) => UserId -> m (Maybe BowBotId)
getBowBotIdByDiscord did = only . map fromOnly <$> queryLog "SELECT `id` FROM `peopleDiscord` WHERE `discord` = ?" (Only did)

getBowBotIdByMinecraft :: (MonadIOReader m r, Has Connection r) => UUID -> m (Maybe BowBotId)
getBowBotIdByMinecraft uuid = only . map fromOnly <$> queryLog "SELECT `id` FROM `peopleMinecraft` WHERE `minecraft` = ?" (Only uuid)