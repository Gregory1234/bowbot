module BowBot.Account.RegisterCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Account.Basic
import BowBot.Minecraft.Arg
import BowBot.Account.Arg
import BowBot.Discord.Utils
import BowBot.BotData.Cached (storeInCache, getFromCache)
import BowBot.Hypixel.Basic (HypixelApi(..))
import BowBot.Hypixel.Leaderboard
import BowBot.Counter.Basic (tryIncreaseCounter)
import BowBot.Account.Register
import BowBot.Discord.Roles
import BowBot.BotData.Info (askInfo, discordGuildIdInfo)
import BowBot.Hypixel.Stats (requestHypixelBowStats)
import Control.Monad.Except
import BowBot.Discord.Account
import BowBot.Discord.Arg
import BowBot.Hypixel.LeaderboardStatus

data RegisterCommandMessages = RegisterCommandMessages { registerAlreadyBelongsMessage :: Text, registerAlreadyBelongsSomeoneElseMessage :: Text, registerAlreadyRegisteredMessage :: Text }

registerCommandBody :: RegisterCommandMessages -> Text -> UserId -> ExceptT Text CommandHandler ()
registerCommandBody RegisterCommandMessages {..} name did = do
  uuid <- liftMaybe thePlayerDoesNotExistMessage =<< mcNameToUUID name
  bacc <- getBowBotAccountByMinecraft uuid
  for_ bacc $ \BowBotAccount {..} -> throwError $ if did `elem` accountDiscords then registerAlreadyBelongsMessage else registerAlreadyBelongsSomeoneElseMessage
  baccdc <- getBowBotAccountByDiscord did
  for_ baccdc $ \_ -> throwError registerAlreadyRegisteredMessage
  cv <- tryIncreaseCounter HypixelApi 1
  stats <- case cv of
    Nothing -> liftMaybe "*The player has never joined Hypixel!*" =<< requestHypixelBowStats uuid
    Just sec -> throwError $ "*Too many requests! Wait another " <> showt sec <> " seconds!*"
  saved :: Maybe MinecraftAccount <- getFromCache uuid
  mc <- case saved of
    Nothing -> do
      names <- liftMaybe thePlayerDoesNotExistMessage =<< mcUUIDToNames uuid
      let newacc = MinecraftAccount { mcUUID = uuid, mcNames = names }
      a <- storeInCache [newacc]
      unless a $ throwError somethingWentWrongMessage
      when a $ void $ setHypixelBowLeaderboardEntryByUUID uuid (hypixelBowStatsToLeaderboards stats)
      pure newacc
    Just acc -> do
      isBanned <- getHypixelIsBannedByUUID uuid
      when (isBanned == NotBanned) $ do
        void $ setHypixelBowLeaderboardEntryByUUID uuid (hypixelBowStatsToLeaderboards stats)
      pure acc
  newacc <- liftMaybe somethingWentWrongMessage =<< createNewBowBotAccount (head $ mcNames mc) did uuid
  applyRolesByBowBotAccount newacc
  lift $ respond "*Registered successfully*"

registerCommand :: Command
registerCommand = Command CommandInfo
  { commandName = "register"
  , commandHelpEntries = [HelpEntry { helpUsage = "register [name]", helpDescription = "register your Minecraft name in Bow Bot", helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 30
  } $ oneArgument' $ \name ->
    lift (envs envSender) >>=
      registerCommandBody RegisterCommandMessages {
          registerAlreadyBelongsMessage = "*That account already belongs to you!*",
          registerAlreadyBelongsSomeoneElseMessage = "*That account already belongs to someone else!*",
          registerAlreadyRegisteredMessage = "*You are already registered!*"
        } name . userId

addCommand :: Command
addCommand = Command CommandInfo
  { commandName = "add"
  , commandHelpEntries = [HelpEntry { helpUsage = "add [discord] [name]", helpDescription = "register someone in Bow Bot", helpGroup = "normal" }]
  , commandPerms = ModLevel
  , commandTimeout = 30
  } $ twoArguments' $ \did name ->
    discordArg did >>=
      registerCommandBody RegisterCommandMessages {
          registerAlreadyBelongsMessage = "*That account already belongs to this user!*",
          registerAlreadyBelongsSomeoneElseMessage = "*That account already belongs to someone else!*",
          registerAlreadyRegisteredMessage = "*That person is not registered!*"
        } name . discordId

addaltCommand :: Command
addaltCommand = Command CommandInfo
  { commandName = "addalt"
  , commandHelpEntries = [HelpEntry { helpUsage = "addalt [discord] [name]", helpDescription = "register someone's alt account in Bow Bot", helpGroup = "normal" }]
  , commandPerms = ModLevel
  , commandTimeout = 30
  } $ twoArguments' $ \did name -> do
    bacc <- liftMaybe thePlayerIsntRegisteredMessage =<< getBowBotAccountByDiscord . discordId =<< discordArg did
    uuid <- liftMaybe thePlayerDoesNotExistMessage =<< mcNameToUUID name
    baccother <- getBowBotAccountByMinecraft uuid
    for_ baccother $ \acc -> throwError $ if bacc == acc then "*That account already belongs to this user!*" else "*That account already belongs to someone else!*"
    cv <- tryIncreaseCounter HypixelApi 1
    stats <- case cv of
      Nothing -> liftMaybe "*The player has never joined Hypixel!*" =<< requestHypixelBowStats uuid
      Just sec -> throwError $ "*Too many requests! Wait another " <> showt sec <> " seconds!*"
    saved :: Maybe MinecraftAccount <- getFromCache uuid
    case saved of -- TODO: remove repetition!
      Nothing -> do
        names <- liftMaybe thePlayerDoesNotExistMessage =<< mcUUIDToNames uuid
        let newacc = MinecraftAccount { mcUUID = uuid, mcNames = names }
        a <- storeInCache [newacc]
        unless a $ throwError somethingWentWrongMessage
        when a $ void $ setHypixelBowLeaderboardEntryByUUID uuid (hypixelBowStatsToLeaderboards stats)
      Just _ -> do
        isBanned <- getHypixelIsBannedByUUID uuid
        when (isBanned == NotBanned) $ do
          void $ setHypixelBowLeaderboardEntryByUUID uuid (hypixelBowStatsToLeaderboards stats)
    newacc <- liftMaybe somethingWentWrongMessage =<< addAltToBowBotAccount (accountBotId bacc) uuid
    applyRolesByBowBotAccount newacc
    lift $ respond "*Registered successfully*"