module BowBot.Command.Utils where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Utils
import Discord.Types
import Control.Monad.Except
import BowBot.Account.Basic
import BowBot.BotData.Info
import BowBot.Minecraft.Basic

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
  mcs' <- getMinecraftListByDiscord (userId user)
  for_ mcs' $ \mcs -> 
    when (mcUUID acc `elem` allMinecrafts mcs) $ do
      prefix <- askInfo discordCommandPrefixInfo
      if mcUUID acc == selectedMinecraft mcs
        then respond "*Tip: you can use most Bow Bot's commands on yourself by not providing your username!*"
        else respond $ "*Tip: you can select this account (for use in Bow Bot's commands without providing a username) using `" <> prefix <> "selectmc " <> (head . mcNames $ acc) <> "`!*"

addMinecraftAccount :: MinecraftAccount -> ExceptT Text CommandHandler ()
addMinecraftAccount acc@MinecraftAccount {..} = do
  acc' <- getMinecraftAccountByUUID mcUUID
  assertIO (isNothing acc')
  respond "**A new Minecraft player discovered! 🥳**"
  storeMinecraftAccount acc
  void $ addMinecraftName (head mcNames) mcUUID

commandMinecraftByNameWithSkipTip :: (MinecraftAccount -> ExceptT Text CommandHandler ()) -> (MinecraftAutocorrect -> ExceptT Text CommandHandler ()) -> Text -> ExceptT Text CommandHandler ()
commandMinecraftByNameWithSkipTip new old n = do
  accBySavedCurrentName <- getMinecraftAccountByCurrentName n
  case accBySavedCurrentName of
    Just acc -> do
      showSelfSkipTip acc
      old $ autocorrectFromAccountDirect acc
    Nothing -> do
      uuidByName <- mojangNameToUUID n
      case uuidByName of
        Just uuid -> do
          accByUUID <- getMinecraftAccountByUUID uuid
          case accByUUID of
            Nothing -> do
              acc <- liftMaybe thePlayerDoesNotExistMessage =<< freshMinecraftAccountByUUID uuid
              new acc
            Just acc -> do
              newName <- liftMaybe somethingWentWrongMessage =<< mojangUUIDToCurrentName uuid
              let newAcc = acc { mcNames = newName : mcNames acc }
              storeMinecraftAccount newAcc
              void $ addMinecraftName newName uuid
              showSelfSkipTip newAcc
              old (autocorrectFromAccountDirect newAcc)
        Nothing -> do
          autocorrect@MinecraftAutocorrect {..} <- liftMaybe thePlayerDoesNotExistMessage =<< minecraftAutocorrect n
          showSelfSkipTip autocorrectAccount
          old autocorrect

commandMinecraftByUUID :: (MinecraftAccount -> ExceptT Text CommandHandler ()) -> (MinecraftAccount -> ExceptT Text CommandHandler ()) -> UUID -> ExceptT Text CommandHandler ()
commandMinecraftByUUID new old uuid = do
  acc' <- getMinecraftAccountByUUID uuid
  case acc' of
    Nothing -> do
      acc <- liftMaybe thePlayerDoesNotExistMessage =<< freshMinecraftAccountByUUID uuid
      new acc
    Just acc ->
      old acc