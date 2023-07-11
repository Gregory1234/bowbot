module BowBot.Stats.Command where

import BowBot.Minecraft.Basic
import BowBot.Command
import Control.Monad.Except
import BowBot.Settings.Basic
import BowBot.Minecraft.Account
import BowBot.Command.Utils
import BowBot.Discord.Utils
import BowBot.Account.Utils
import BowBot.Minecraft.IsBanned


data StatsCommandSettings a = StatsCommandSettings
  { requestStats :: Bool -> UUID -> ExceptT Text CommandHandler a
  , shouldAddAccount :: a -> Bool
  , updateStats :: UUID -> a -> ExceptT Text CommandHandler ()
  , showStats :: Settings -> a -> Text
  , getIsBanned :: UUID -> ExceptT Text CommandHandler IsBanned
  }

statsCommandTemplate :: forall a. StatsCommandSettings a -> SettingsSource -> Text -> Text -> Command
statsCommandTemplate StatsCommandSettings {..} src name desc = Command CommandInfo
  { commandName = name
  , commandHelpEntries = [HelpEntry { helpUsage = name <> " [name]", helpDescription = desc, helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument $ \case
    Nothing -> do
      did <- userId <$> envs envSender
      acc <- liftMaybe youArentRegisteredMessage =<< getSelectedMinecraftByDiscord did
      stats <- requestStats True (mcUUID acc)
      displayStats (minecraftAccountToHeader acc Nothing) stats
      updateStatsUnlessBanned (mcUUID acc) stats
    Just (uuidFromString -> Just uuid) -> do
      commandMinecraftByUUID handlerNewAccount (handlerOldAccount . autocorrectFromAccountDirect) uuid
    Just (discordIdFromString -> Just did) -> do
      acc <- liftMaybe theUserIsntRegisteredMessage =<< getSelectedMinecraftByDiscord did
      handlerOldAccount (autocorrectFromAccountDirect acc)
    Just n -> do
      commandMinecraftByNameWithSkipTip handlerNewAccount handlerOldAccount n
  where
    handlerOldAccount :: MinecraftAutocorrect -> ExceptT Text CommandHandler ()
    handlerOldAccount ac@MinecraftAutocorrect {autocorrectAccount = acc} = do
      stats <- requestStats False (mcUUID acc)
      displayStats (minecraftAutocorrectToHeader ac) stats
      updateStatsUnlessBanned (mcUUID acc) stats
    handlerNewAccount :: MinecraftAccount -> ExceptT Text CommandHandler ()
    handlerNewAccount acc = do
      stats <- requestStats False (mcUUID acc)
      let toAdd = shouldAddAccount stats
      when toAdd $ addMinecraftAccount acc
      displayStats (minecraftAccountToHeader acc Nothing) stats
      when toAdd $ updateStats (mcUUID acc) stats
    displayStats :: Text -> a -> ExceptT Text CommandHandler ()
    displayStats header stats = do
      user <- envs envSender
      settings <- getSettingsFromSource src (userId user)
      respond $ header <> showStats settings stats
    updateStatsUnlessBanned :: UUID -> a -> ExceptT Text CommandHandler ()
    updateStatsUnlessBanned uuid stats = do
      isBanned <- getIsBanned uuid
      when (isBanned == NotBanned) $ updateStats uuid stats