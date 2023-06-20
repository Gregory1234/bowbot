module BowBot.Account.Utils where

import BowBot.Account.Basic
import BowBot.Minecraft.Account
import BowBot.Discord.Utils
import BowBot.BotData.Cached
import BowBot.DB.Basic

getSelectedMinecraftByDiscord :: (MonadIOBotData m d r, HasCache MinecraftAccount d, Has Connection r) => UserId -> m (Maybe MinecraftAccount)
getSelectedMinecraftByDiscord did = runMaybeT $ do
  selectedMinecraft <- liftMaybe () =<< getSelectedMinecraftUUIDByDiscord did
  liftMaybe () =<< getFromCache @MinecraftAccount selectedMinecraft
  