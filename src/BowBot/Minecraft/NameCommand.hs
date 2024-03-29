module BowBot.Minecraft.NameCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import qualified Data.Text as T
import BowBot.Command.Utils
import BowBot.Discord.Utils
import BowBot.Minecraft.Basic
import Control.Monad.Except
import BowBot.Account.Utils

nameCommand :: Command
nameCommand = Command CommandInfo
  { commandName = "n"
  , commandHelpEntries = [HelpEntry { helpUsage = "n [name]", helpDescription = "show player's Minecraft name history", helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument $ \case
    Nothing -> do
      did <- userId <$> envs envSender
      acc <- liftMaybe youArentRegisteredMessage =<< getSelectedMinecraftByDiscord did
      handler (autocorrectFromAccountDirect acc)
    Just (uuidFromString -> Just uuid) -> do
      acc <- liftMaybe thePlayerDoesNotExistMessage =<< getMinecraftAccountByUUID uuid
      handler (autocorrectFromAccountDirect acc)
    Just (discordIdFromString -> Just did) -> do
      acc <- liftMaybe theUserIsntRegisteredMessage =<< getSelectedMinecraftByDiscord did
      handler (autocorrectFromAccountDirect acc)
    Just n -> do
      ac <- liftMaybe thePlayerDoesNotExistMessage =<< minecraftAutocorrect n
      handler ac
  where
    handler :: MinecraftAutocorrect -> ExceptT Text CommandHandler ()
    handler ac@MinecraftAutocorrect {..} = do
      let header = (if autocorrectIsDirect then "Name history of " else "") <> minecraftAutocorrectToHeader ac
      respond $ header <> "```\n" <> T.unlines (mcNames autocorrectAccount) <> "```"