module BowBot.Account.Utils where

import BowBot.Account.Basic
import BowBot.Minecraft.Account
import BowBot.Discord.Utils
import BowBot.BotData.Cached
import BowBot.DB.Basic
import BowBot.Discord.Account

getSelectedMinecraftByDiscord :: (MonadIOBotData m d r, HasCache MinecraftAccount d, Has Connection r) => UserId -> m (Maybe MinecraftAccount)
getSelectedMinecraftByDiscord did = runMaybeT $ do
  selectedMinecraft <- liftMaybe () =<< getSelectedMinecraftUUIDByDiscord did
  liftMaybe () =<< getFromCache @MinecraftAccount selectedMinecraft

getDiscordAccountsByBowBotId :: (MonadIOReader m r, Has Connection r) => BowBotId -> m [DiscordAccount]
getDiscordAccountsByBowBotId bid = queryLog "SELECT `discord`.`id`, `discord`.`name`, `discord`.`discriminator`, `discord`.`nickname`, `discord`.`member` FROM `discord` JOIN `peopleDiscord` ON `peopleDiscord`.`discord` = `discord`.`id` WHERE `peopleDiscord`.`id` = ?" (Only bid)