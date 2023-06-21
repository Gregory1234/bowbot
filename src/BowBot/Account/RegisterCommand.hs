module BowBot.Account.RegisterCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Account.Basic
import BowBot.Discord.Utils
import BowBot.Hypixel.Leaderboard
import BowBot.Account.Register
import BowBot.Discord.Roles
import Control.Monad.Except
import BowBot.Command.Utils
import BowBot.Hypixel.CommandUtils

data RegisterCommandSettings = RegisterCommandSettings
  { alreadyRegisteredByYouMsg :: Text
  , alreadyRegisteredBySomeoneElseMsg :: Text
  , alreadyRegisteredMsg :: Maybe Text
  }

registerCommandHandler :: RegisterCommandSettings -> Text -> UserId -> ExceptT Text CommandHandler ()
registerCommandHandler RegisterCommandSettings {..} name did = do
  uuid <- liftMaybe thePlayerDoesNotExistMessage =<< mcNameToUUID name
  dcsOfUUID <- getDiscordIdsByMinecraft uuid
  unless (null dcsOfUUID) $ throwError $ if did `elem` dcsOfUUID then alreadyRegisteredByYouMsg else alreadyRegisteredBySomeoneElseMsg
  bbacc' <- getBowBotIdByDiscord did
  newid <- case (bbacc', alreadyRegisteredMsg) of
    (Just _, Just msg) -> throwError msg
    (Just bid, Nothing) -> do
      void $ fullAddMinecraft uuid
      a <- addAltToBowBotAccount bid uuid
      unless a $ throwError somethingWentWrongMessage
      return bid
    (Nothing, _) -> do
      mc <- fullAddMinecraft uuid
      liftMaybe somethingWentWrongMessage =<< createNewBowBotAccount (head $ mcNames mc) did uuid
  applyRolesByBowBotId newid
  respond "*Registered successfully*"
    where
      fullAddMinecraft uuid = do
        acc' <- getMinecraftAccountByUUID uuid
        case acc' of
          Nothing -> do
            acc <- liftMaybe thePlayerDoesNotExistMessage =<< freshMinecraftAccountByUUID uuid
            stats <- liftMaybe thePlayerNeverJoinedHypixelMessage =<< hypixelSafeRequestStats uuid
            addMinecraftAccount acc
            void $ setHypixelBowLeaderboardEntryByUUID uuid (hypixelBowStatsToLeaderboards stats)
            return acc
          Just acc -> return acc

registerCommand :: Command
registerCommand = Command CommandInfo
  { commandName = "register"
  , commandHelpEntries = [HelpEntry { helpUsage = "register [name]", helpDescription = "register your Minecraft name in Bow Bot", helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 30
  } $ oneArgument $ \name -> do
    did <- userId <$> envs envSender
    registerCommandHandler RegisterCommandSettings
      { alreadyRegisteredByYouMsg = "*That account already belongs to you!*"
      , alreadyRegisteredBySomeoneElseMsg = "*That account already belongs to someone else!*"
      , alreadyRegisteredMsg = Just "*You are already registered!*"
      } name did

addCommand :: Command
addCommand = Command CommandInfo
  { commandName = "add"
  , commandHelpEntries = [HelpEntry { helpUsage = "add [discord] [name]", helpDescription = "register someone in Bow Bot", helpGroup = "normal" }]
  , commandPerms = ModLevel
  , commandTimeout = 30
  } $ twoArguments $ \did' name -> do
    did <- liftMaybe theDiscordIdIsInvalid $ discordIdFromString did'
    registerCommandHandler RegisterCommandSettings
      { alreadyRegisteredByYouMsg = "*That account already belongs to this user!*"
      , alreadyRegisteredBySomeoneElseMsg = "*That account already belongs to someone else!*"
      , alreadyRegisteredMsg = Nothing
      } name did