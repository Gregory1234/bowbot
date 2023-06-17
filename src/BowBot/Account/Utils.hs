module BowBot.Account.Utils where

import BowBot.Account.Basic
import BowBot.Minecraft.Account
import BowBot.Discord.Utils
import BowBot.BotData.Cached

getSelectedMinecraftByDiscord :: (MonadIOBotData m d r, HasCaches '[BowBotAccount, MinecraftAccount] d) => UserId -> m (Maybe MinecraftAccount)
getSelectedMinecraftByDiscord did = runMaybeT $ do
  bbacc <- liftMaybe () =<< getBowBotAccountByDiscord did
  liftMaybe () =<< getFromCache @MinecraftAccount (accountSelectedMinecraft bbacc) 
  