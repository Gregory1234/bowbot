{-# LANGUAGE TypeFamilies #-}

module BowBot.Account.Basic where

import BowBot.Minecraft.Basic (UUID(..))
import Discord.Internal.Rest (UserId)
import BowBot.DB.Basic
import BowBot.Utils
import BowBot.Discord.Orphans ()
import Data.Hashable (Hashable)

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
getSelectedMinecraftUUIDByDiscord did = fmap fromOnly <$> queryOnlyLog "SELECT `minecraft_uuid` FROM `account_minecraft` JOIN `account_discord` ON `account_minecraft`.`account_id`=`account_discord`.`account_id` WHERE `account_discord`.`discord_id` = ? AND `account_minecraft`.`selected` = 1" (Only did)

getMinecraftUUIDsByDiscord :: (MonadIOReader m r, Has Connection r) => UserId -> m [UUID]
getMinecraftUUIDsByDiscord did = map fromOnly <$> queryLog "SELECT `minecraft_uuid` FROM `account_minecraft` JOIN `account_discord` ON `account_minecraft`.`account_id`=`account_discord`.`account_id` WHERE `account_discord`.`discord_id` = ?" (Only did)

getMinecraftListByDiscord :: (MonadIOReader m r, Has Connection r) => UserId -> m (Maybe MinecraftList)
getMinecraftListByDiscord did = minecraftListFromList <$> queryLog "SELECT `minecraft_uuid`,`selected` FROM `account_minecraft` JOIN `account_discord` ON `account_minecraft`.`account_id`=`account_discord`.`account_id` WHERE `account_discord`.`discord_id` = ?" (Only did)

getMinecraftListByBowBotId :: (MonadIOReader m r, Has Connection r) => BowBotId -> m (Maybe MinecraftList)
getMinecraftListByBowBotId bid = minecraftListFromList <$> queryLog "SELECT `minecraft_uuid`,`selected` FROM `account_minecraft` WHERE `account_id` = ?" (Only bid)

getDiscordIdsByMinecraft :: (MonadIOReader m r, Has Connection r) => UUID -> m [UserId]
getDiscordIdsByMinecraft uuid = map fromOnly <$> queryLog "SELECT `discord_id` FROM `account_discord` JOIN `account_minecraft` ON `account_minecraft`.`account_id`=`account_discord`.`account_id` WHERE `account_minecraft`.`minecraft_uuid` = ?" (Only uuid)

getDiscordIdsByBowBotId :: (MonadIOReader m r, Has Connection r) => BowBotId -> m [UserId]
getDiscordIdsByBowBotId bid = map fromOnly <$> queryLog "SELECT `discord_id` FROM `account_discord` WHERE `account_id` = ?" (Only bid)

getDiscordIdsByDiscord :: (MonadIOReader m r, Has Connection r) => UserId -> m [UserId]
getDiscordIdsByDiscord uuid = map fromOnly <$> queryLog "SELECT `account_discord2`.`discord_id` FROM `account_discord` JOIN `account_discord` AS `account_discord2` ON `account_discord`.`account_id` = `account_discord2`.`account_id` WHERE `account_discord`.`discord_id` = ?" (Only uuid)

getBowBotIdByDiscord :: (MonadIOReader m r, Has Connection r) => UserId -> m (Maybe BowBotId)
getBowBotIdByDiscord did = fmap fromOnly <$> queryOnlyLog "SELECT `account_id` FROM `account_discord` WHERE `discord_id` = ?" (Only did)

getBowBotIdByMinecraft :: (MonadIOReader m r, Has Connection r) => UUID -> m (Maybe BowBotId)
getBowBotIdByMinecraft uuid = fmap fromOnly <$> queryOnlyLog "SELECT `account_id` FROM `account_minecraft` WHERE `minecraft_uuid` = ?" (Only uuid)