module BowBot.Minecraft.UrlCommand where

import BowBot.Command
import BowBot.Minecraft.Basic
import BowBot.Minecraft.Account
import BowBot.Command.Utils
import BowBot.Discord.Utils
import BowBot.Account.Utils

urlCommand :: Text -> Text -> (UUID -> Text) -> Command
urlCommand name desc url = Command CommandInfo
  { commandName = name
  , commandHelpEntries = [HelpEntry { helpUsage = name <> " [name]", helpDescription = desc, helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument $ \case
    Nothing -> do
      did <- userId <$> envs envSender
      acc <- liftMaybe youArentRegisteredMessage =<< getSelectedMinecraftByDiscord did
      respond $ url $ mcUUID acc
    Just (uuidFromString -> Just uuid) -> do
      respond $ url uuid
    Just (discordIdFromString -> Just did) -> do
      acc <- liftMaybe theUserIsntRegisteredMessage =<< getSelectedMinecraftByDiscord did
      respond $ url $ mcUUID acc
    Just n -> do
      commandMinecraftAutocorrectByNameWithSkipTip (respond . url . mcUUID) (respond . url . mcUUID . autocorrectAccount) n