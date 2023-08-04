{-# LANGUAGE QuasiQuotes #-}

module BowBot.Account.Basic where

import BowBot.Minecraft.Basic (UUID(..))
import Discord.Internal.Rest (UserId)
import BowBot.DB.Typed
import BowBot.Utils
import BowBot.Discord.Orphans ()
import Data.Hashable (Hashable)

newtype BowBotId = BowBotId { unBowBotId :: Integer }
  deriving stock (Show, Eq, Ord)
  deriving newtype (Hashable, Param, Result, ToMysql, FromMysql)

data MinecraftList = MinecraftList { selectedMinecraft :: UUID, allMinecrafts :: [UUID] }

minecraftListFromList :: [(UUID, Bool)] -> Maybe MinecraftList
minecraftListFromList mcs = fmap (\selectedMinecraft -> MinecraftList {..}) selectedMinecraft'
  where
    selectedMinecraft' = fmap fst $ only $ filter snd mcs
    allMinecrafts = map fst mcs

getSelectedMinecraftUUIDByDiscord :: (MonadIOReader m r, Has Connection r) => UserId -> m (Maybe UUID)
getSelectedMinecraftUUIDByDiscord did = queryOnlyLogT [mysql|SELECT `minecraft_uuid` FROM `account_minecraft` 
  JOIN `account_discord` ON `account_id`=`account_minecraft`.`account_id` WHERE `account_discord`.`discord_id` = did AND `account_minecraft`.`selected`|]

getMinecraftUUIDsByDiscord :: (MonadIOReader m r, Has Connection r) => UserId -> m [UUID]
getMinecraftUUIDsByDiscord did = queryLogT [mysql|SELECT `minecraft_uuid` FROM `account_minecraft` 
  JOIN `account_discord` ON `account_id`=`account_minecraft`.`account_id` WHERE `account_discord`.`discord_id` = did|]

getMinecraftListByDiscord :: (MonadIOReader m r, Has Connection r) => UserId -> m (Maybe MinecraftList)
getMinecraftListByDiscord did = minecraftListFromList <$> queryLogT [mysql|SELECT `minecraft_uuid`,`selected` FROM `account_minecraft` 
  JOIN `account_discord` ON `account_id`=`account_minecraft`.`account_id` WHERE `account_discord`.`discord_id` = did|]

getMinecraftListByBowBotId :: (MonadIOReader m r, Has Connection r) => BowBotId -> m (Maybe MinecraftList)
getMinecraftListByBowBotId bid = minecraftListFromList <$> queryLogT [mysql|SELECT `minecraft_uuid`,`selected` FROM `account_minecraft` WHERE `account_id` = bid|]

getDiscordIdsByMinecraft :: (MonadIOReader m r, Has Connection r) => UUID -> m [UserId]
getDiscordIdsByMinecraft uuid = queryLogT [mysql|SELECT `discord_id` FROM `account_discord` 
  JOIN `account_minecraft` ON `account_id`=`account_discord`.`account_id` WHERE `account_minecraft`.`minecraft_uuid` = uuid|]

getDiscordIdsByBowBotId :: (MonadIOReader m r, Has Connection r) => BowBotId -> m [UserId]
getDiscordIdsByBowBotId bid = queryLogT [mysql|SELECT `discord_id` FROM `account_discord` WHERE `account_id` = bid|]

getDiscordIdsByDiscord :: (MonadIOReader m r, Has Connection r) => UserId -> m [UserId]
getDiscordIdsByDiscord did = queryLogT [mysql|SELECT `account_discord2`.`discord_id` FROM `account_discord` 
  JOIN `account_discord` AS `account_discord2` ON `account_id` = `account_discord`.`account_id` WHERE `account_discord`.`discord_id` = did|]

getBowBotIdByDiscord :: (MonadIOReader m r, Has Connection r) => UserId -> m (Maybe BowBotId)
getBowBotIdByDiscord did = queryOnlyLogT [mysql|SELECT `account_id` FROM `account_discord` WHERE `discord_id` = did|]

getBowBotIdByMinecraft :: (MonadIOReader m r, Has Connection r) => UUID -> m (Maybe BowBotId)
getBowBotIdByMinecraft uuid = queryOnlyLogT [mysql|SELECT `account_id` FROM `account_minecraft` WHERE `minecraft_uuid` = uuid|]