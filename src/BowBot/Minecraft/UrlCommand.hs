module BowBot.Minecraft.UrlCommand where

import BowBot.Command
import BowBot.Minecraft.Basic
import BowBot.Minecraft.Account
import BowBot.Command.Utils
import BowBot.Discord.Utils
import BowBot.Account.Utils
import Control.Monad.Except
import BowBot.Network.Basic
import Data.ByteString (toStrict)

urlImageCommand :: Text -> Text -> Text -> (UUID -> Text) -> Command
urlImageCommand name desc filename url = Command CommandInfo
  { commandName = name
  , commandHelpEntries = [HelpEntry { helpUsage = name <> " [name]", helpDescription = desc, helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument $ \case
    Nothing -> do
      did <- userId <$> envs envSender
      acc <- liftMaybe youArentRegisteredMessage =<< getSelectedMinecraftByDiscord did
      handler $ mcUUID acc
    Just (uuidFromString -> Just uuid) -> do
      handler uuid
    Just (discordIdFromString -> Just did) -> do
      acc <- liftMaybe theUserIsntRegisteredMessage =<< getSelectedMinecraftByDiscord did
      handler $ mcUUID acc
    Just n -> do
      commandMinecraftAutocorrectByNameWithSkipTip (handler . mcUUID) (handler . mcUUID . autocorrectAccount) n
  where
    handler :: UUID -> ExceptT Text CommandHandler ()
    handler uuid = do
      let u = url uuid
      img <- sendRequestTo u u
      respondFileBin filename (toStrict img)