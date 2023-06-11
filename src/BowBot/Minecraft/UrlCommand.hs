module BowBot.Minecraft.UrlCommand where

import BowBot.Command
import BowBot.Minecraft.Basic
import BowBot.Minecraft.Account
import BowBot.Command.Utils
import BowBot.Discord.Utils

urlCommand :: Text -> Text -> (UUID -> Text) -> Command
urlCommand name desc url = Command CommandInfo
  { commandName = name
  , commandHelpEntries = [HelpEntry { helpUsage = name <> " [name]", helpDescription = desc, helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument $ \case
    Nothing -> do
      acc <- commandSelectedMinecraftByDiscordSelf
      respond $ url $ mcUUID acc
    Just (uuidFromString -> Just uuid) -> do
      respond $ url uuid
    Just (discordIdFromString -> Just did) -> do
      acc <- commandSelectedMinecraftByDiscord did
      respond $ url $ mcUUID acc
    Just n -> do
      ac <- liftMaybe thePlayerDoesNotExistMessage =<< minecraftAutocorrect n
      respond $ url $ mcUUID $ autocorrectAccount ac