module BowBot.Command.Utils where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Utils
import Discord.Types
import BowBot.BotData.Cached
import Control.Monad.Except
import BowBot.Account.Basic
import BowBot.BotData.Info
import BowBot.Minecraft.Basic (UUID)

youArentRegisteredMessage :: Text
youArentRegisteredMessage = "*You aren't registered! To register, type `?register yourign`.*"

theUserIsntRegisteredMessage :: Text
theUserIsntRegisteredMessage = "*The user isn't registered!*"

thePlayerDoesNotExistMessage :: Text
thePlayerDoesNotExistMessage = "*The player doesn't exist!*"

theDiscordIdIsInvalid :: Text
theDiscordIdIsInvalid = "*The discord id is invalid!*"

showSelfSkipTip :: MinecraftAccount -> ExceptT Text CommandHandler ()
showSelfSkipTip acc = do
  user <- envs envSender
  bbaccMaybe <- getBowBotAccountByDiscord (userId user)
  for_ bbaccMaybe $ \BowBotAccount {..} -> do
    when (mcUUID acc `elem` accountMinecrafts) $ do
      prefix <- askInfo discordCommandPrefixInfo
      if mcUUID acc == accountSelectedMinecraft
        then respond "*Tip: you can use most Bow Bot's commands on yourself by not providing your username!*"
        else respond $ "*Tip: you can select this account (for use in Bow Bot's commands without providing a username) using `" <> prefix <> "selectmc " <> (head . mcNames $ acc) <> "`!*"

addMinecraftAccount :: MinecraftAccount -> ExceptT Text CommandHandler ()
addMinecraftAccount acc@MinecraftAccount {..} = do
  respond "**A new Minecraft player discovered! 🥳**"
  void $ storeInCache [acc]
  void $ addMinecraftName (head mcNames) mcUUID

commandMinecraftByNameWithSkipTip :: (MinecraftAccount -> ExceptT Text CommandHandler ()) -> (MinecraftAutocorrect -> ExceptT Text CommandHandler ()) -> Text -> ExceptT Text CommandHandler ()
commandMinecraftByNameWithSkipTip new old n = do
  autocorrect <- minecraftAutocorrect n
  case autocorrect of
    Nothing -> do
      acc <- liftMaybe thePlayerDoesNotExistMessage =<< freshMinecraftAccountByName n
      new acc
    Just ac@MinecraftAutocorrect {..} -> do
      showSelfSkipTip autocorrectAccount
      old ac

commandMinecraftByUUID :: (MinecraftAccount -> ExceptT Text CommandHandler ()) -> (MinecraftAccount -> ExceptT Text CommandHandler ()) -> UUID -> ExceptT Text CommandHandler ()
commandMinecraftByUUID new old uuid = do
  acc' <- getFromCache @MinecraftAccount uuid
  case acc' of
    Nothing -> do
      acc <- liftMaybe thePlayerDoesNotExistMessage =<< freshMinecraftAccountByUUID uuid
      new acc
    Just acc ->
      old acc