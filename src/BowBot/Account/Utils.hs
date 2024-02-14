{-# LANGUAGE QuasiQuotes #-}

module BowBot.Account.Utils where

import BowBot.Account.Basic
import BowBot.Minecraft.Account
import BowBot.Discord.Utils
import BowBot.DB.Basic
import BowBot.Discord.Account
import BowBot.Minecraft.Basic

getSelectedMinecraftByDiscord :: (MonadIOReader m r, Has SafeMysqlConn r) => UserId -> m (Maybe MinecraftAccount)
getSelectedMinecraftByDiscord did = queryOnlyLog [mysql|SELECT MinecraftAccount FROM `minecraft` JOIN `account_minecraft` ON `minecraft_uuid` = `minecraft`.`uuid` 
  JOIN `account_discord` ON `account_id` = `account_minecraft`.`account_id` WHERE `account_discord`.`discord_id` = did AND `account_minecraft`.`selected`|]

getSelectedMinecraftByBowBotId :: (MonadIOReader m r, Has SafeMysqlConn r) => BowBotId -> m (Maybe MinecraftAccount)
getSelectedMinecraftByBowBotId bid = queryOnlyLog [mysql|SELECT MinecraftAccount FROM `minecraft` JOIN `account_minecraft` ON `minecraft_uuid` = `minecraft`.`uuid` 
  WHERE `account_minecraft`.`account_id` = bid AND `account_minecraft`.`selected`|]

getDiscordAccountsByBowBotId :: (MonadIOReader m r, Has SafeMysqlConn r) => BowBotId -> m [DiscordAccount]
getDiscordAccountsByBowBotId bid = queryLog [mysql|SELECT DiscordAccount FROM `discord` JOIN `account_discord` ON `discord_id` = `discord`.`id` WHERE `account_discord`.`account_id` = bid|]

getMinecraftAccountsByBowBotId :: (MonadIOReader m r, Has SafeMysqlConn r) => BowBotId -> m [(MinecraftAccount, Bool)]
getMinecraftAccountsByBowBotId bid = queryLog [mysql|SELECT MinecraftAccount, `account_minecraft`.`selected` FROM `minecraft` 
  JOIN `account_minecraft` ON `minecraft_uuid` = `minecraft`.`uuid` WHERE `account_minecraft`.`account_id` = bid|]