module BowBot.Account.Utils where

import BowBot.Account.Basic
import BowBot.Minecraft.Account
import BowBot.Discord.Utils
import BowBot.DB.Basic
import BowBot.Discord.Account
import Data.Bifunctor (second)

getSelectedMinecraftByDiscord :: (MonadIOReader m r, Has Connection r) => UserId -> m (Maybe MinecraftAccount)
getSelectedMinecraftByDiscord did = runMaybeT $ do
  selectedMinecraft <- liftMaybe () =<< getSelectedMinecraftUUIDByDiscord did
  liftMaybe () =<< getMinecraftAccountByUUID selectedMinecraft -- TODO: do it directly?

getDiscordAccountsByBowBotId :: (MonadIOReader m r, Has Connection r) => BowBotId -> m [DiscordAccount]
getDiscordAccountsByBowBotId bid = queryLog "SELECT `discord`.`id`, `discord`.`name`, `discord`.`discriminator`, `discord`.`nickname`, `discord`.`member` FROM `discord` JOIN `account_discord` ON `account_discord`.`discord_id` = `discord`.`id` WHERE `account_discord`.`account_id` = ?" bid

getMinecraftAccountsByBowBotId :: (MonadIOReader m r, Has Connection r) => BowBotId -> m [(MinecraftAccount, Bool)]
getMinecraftAccountsByBowBotId bid = queryLog "SELECT `minecraft`.`uuid`, `minecraft`.`names`, `account_minecraft`.`selected` FROM `minecraft` JOIN `account_minecraft` ON `account_minecraft`.`minecraft_uuid` = `minecraft`.`uuid` WHERE `account_minecraft`.`account_id` = ?" bid