module BowBot.Command.Tips where

import BowBot.Minecraft.Account
import BowBot.Command.Handler
import BowBot.Minecraft.Arg
import BowBot.Account.Basic
import BowBot.Discord.Utils
import Control.Monad.Except
import BowBot.BotData.Info
import BowBot.BotData.Cached

minecraftArgFullConstraintWithSkipTip :: (MinecraftAccount -> ExceptT Text CommandHandler (MinecraftConstraintResponse, a)) -> Maybe Text -> ExceptT Text CommandHandler (MinecraftResponse, a)
minecraftArgFullConstraintWithSkipTip constraint s = do
  did <- userId <$> lift (envs envSender)
  acc <- getBowBotAccountByDiscord did
  ret <- minecraftArgFullConstraint constraint did s
  if isJust s && fmap accountSelectedMinecraft acc == Just (mcUUID . mcResponseAccount . fst $ ret)
    then respond "*Tip: you can use most Bow Bot's commands on yourself by not providing your username!*"
    else do
      prefix <- askInfo discordCommandPrefixInfo
      when (isJust s && (mcUUID . mcResponseAccount . fst $ ret) `elem` maybe [] accountMinecrafts acc) $ respond $ "*Tip: you can select this account (for use in Bow Bot's commands without providing a username) using `" <> prefix <> "selectmc " <> (head . mcNames . mcResponseAccount . fst $ ret) <> "`!*"
  return ret

minecraftNewAccountTip :: MinecraftAccount -> CommandHandler ()
minecraftNewAccountTip MinecraftAccount {..} = do
  acc <- getFromCache @MinecraftAccount mcUUID
  when (isNothing acc) $ respond "**A new Minecraft player discovered! 🥳**"