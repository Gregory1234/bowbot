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
getDiscordAccountsByBowBotId bid = queryLog "SELECT `discord`.`id`, `discord`.`name`, `discord`.`discriminator`, `discord`.`nickname`, `discord`.`member` FROM `discord` JOIN `peopleDiscord` ON `peopleDiscord`.`discord` = `discord`.`id` WHERE `peopleDiscord`.`id` = ?" (Only bid)

getMinecraftAccountsByBowBotId :: (MonadIOReader m r, Has Connection r) => BowBotId -> m [(MinecraftAccount, Bool)]
getMinecraftAccountsByBowBotId bid = map (second fromOnly . fromConcat) <$> queryLog "SELECT `minecraft`.`uuid`, `minecraft`.`names`, `peopleMinecraft`.`selected` FROM `minecraft` JOIN `peopleMinecraft` ON `peopleMinecraft`.`minecraft` = `minecraft`.`uuid` WHERE `peopleMinecraft`.`id` = ?" (Only bid)