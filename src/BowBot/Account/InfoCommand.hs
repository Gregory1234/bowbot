module BowBot.Account.InfoCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Utils
import Discord.Types
import BowBot.Minecraft.Basic
import BowBot.Discord.Utils
import Control.Monad.Except
import BowBot.Account.Basic
import BowBot.Discord.Account
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import BowBot.BotData.Cached
import BowBot.Command.Utils

infoCommand :: Command
infoCommand = Command CommandInfo
  { commandName = "i"
  , commandHelpEntries = [HelpEntry { helpUsage = "i [name]", helpDescription = "show info about a player", helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument $ \case -- TODO: show more info
    Nothing -> do
      did <- userId <$> envs envSender
      handleDiscord True did
    Just (uuidFromString -> Just uuid) -> do
      acc <- liftMaybe theUserIsntRegisteredMessage =<< getFromCache @MinecraftAccount uuid
      handleMinecraft $ autocorrectFromAccountDirect acc
    Just (discordIdFromString -> Just did) -> handleDiscord False did
    Just n -> do
      ac <- liftMaybe theUserIsntRegisteredMessage =<< minecraftAutocorrect n
      handleMinecraft ac
  where
    handleDiscord :: Bool -> UserId -> ExceptT Text CommandHandler ()
    handleDiscord self did = do
      bid' <- getBowBotIdByDiscord did
      acc' <- getFromCache @DiscordAccount did
      case (bid', acc') of
        (Just bid, Just acc) -> displayInfo (showDiscordNameDiscord (discordName acc) <> ":\n") bid
        _ -> throwError $ if self then youArentRegisteredMessage else theUserIsntRegisteredMessage
    handleMinecraft :: MinecraftAutocorrect -> ExceptT Text CommandHandler ()
    handleMinecraft ac = do
      let uuid = mcUUID (autocorrectAccount ac)
      bid <- liftMaybe theUserIsntRegisteredMessage =<< getBowBotIdByMinecraft uuid
      displayInfo (minecraftAutocorrectToHeader ac) bid
    displayInfo :: Text -> BowBotId -> ExceptT Text CommandHandler ()
    displayInfo header bid = do
      dcs <- getDiscordIdsByBowBotId bid
      mcs <- liftMaybe somethingWentWrongMessage =<< getMinecraftListByBowBotId bid
      mc <- getCacheMap
      dc <- getCacheMap
      let mcAccs = T.unlines $ map (\uuid -> let MinecraftAccount {..} = mc HM.! uuid in (if mcUUID == selectedMinecraft mcs  then "*" else " ") <> head mcNames <> " (" <> uuidString mcUUID <> ")") (allMinecrafts mcs)
      let dcAccs = T.unlines $ map (\did -> let DiscordAccount {..} = dc HM.! did in (if discordIsMember then "*" else " ") <> showDiscordName discordName <> ", id " <> showt discordId ) dcs
      let infos = [ "- Bow Bot id: " <> showt (unBowBotId bid)
                  , "- Minecraft accounts:\n```\n" <> mcAccs <> "```"
                  , "- Discord accounts:\n```\n" <> dcAccs <> "```"]
      respond $ header <> T.unlines infos