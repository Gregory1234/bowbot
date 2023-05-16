module BowBot.Command.Tips where

import BowBot.Minecraft.Account
import BowBot.Command.Handler
import BowBot.Account.Basic
import BowBot.Discord.Utils
import Control.Monad.Except
import BowBot.BotData.Info
import BowBot.BotData.Cached

minecraftNewAccountTip :: MinecraftAccount -> ExceptT Text CommandHandler ()
minecraftNewAccountTip MinecraftAccount {..} = do
  acc <- getFromCache @MinecraftAccount mcUUID
  when (isNothing acc) $ respond "**A new Minecraft player discovered! 🥳**"