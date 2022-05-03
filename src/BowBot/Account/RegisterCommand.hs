{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module BowBot.Account.RegisterCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Account.Basic
import BowBot.Minecraft.Arg
import BowBot.Account.Arg
import BowBot.Discord.Utils
import BowBot.BotData.Cached (storeInCache, getFromCache, storeInCacheIndexed)
import BowBot.Hypixel.Basic (HypixelApi)
import BowBot.Hypixel.Leaderboard (hypixelBowStatsToLeaderboards)
import Data.Data
import BowBot.BotData.Counter (tryIncreaseCounter)
import BowBot.Account.Register
import BowBot.Discord.Roles (updateRoles)
import BowBot.BotData.Info (hInfoDB, discordGuildIdInfo)
import BowBot.Hypixel.Stats (requestHypixelBowStats)
import Control.Monad.Except
import BowBot.Discord.Account (DiscordAccount)

data RegisterCommandMessages = RegisterCommandMessages { registerAlreadyBelongsMessage :: String, registerAlreadyBelongsSomeoneElseMessage :: String, registerAlreadyRegisteredMessage :: String }

registerCommandBody :: RegisterCommandMessages -> String -> UserId -> ExceptT String CommandHandler ()
registerCommandBody RegisterCommandMessages {..} name did = do
  uuid <- liftMaybe thePlayerDoesNotExistMessage =<< mcNameToUUID name
  bacc <- getBowBotAccountByMinecraft uuid
  for_ bacc $ \BowBotAccount {..} -> throwError $ if did `elem` accountDiscords then registerAlreadyBelongsMessage else registerAlreadyBelongsSomeoneElseMessage
  baccdc <- getBowBotAccountByDiscord did
  for_ baccdc $ \_ -> throwError registerAlreadyRegisteredMessage
  cv <- tryIncreaseCounter (Proxy @HypixelApi) 1
  stats <- case cv of
    Nothing -> liftMaybe "*The player has never joined Hypixel!*" =<< requestHypixelBowStats uuid
    Just sec -> throwError $ "*Too many requests! Wait another " ++ show sec ++ " seconds!*"
  saved <- getFromCache (Proxy @MinecraftAccount) uuid
  mc <- case saved of
    Nothing -> do
      names <- liftMaybe thePlayerDoesNotExistMessage =<< mcUUIDToNames uuid
      let newacc = MinecraftAccount { mcUUID = uuid, mcNames = names, mcHypixelBow = NotBanned, mcHypixelWatchlist = False }
      a <- storeInCache [newacc]
      unless a $ throwError somethingWentWrongMessage
      when a $ void $ storeInCacheIndexed [(uuid, hypixelBowStatsToLeaderboards stats)]
      pure newacc
    Just acc -> do
      when (mcHypixelBow acc == NotBanned) $ void $ storeInCacheIndexed [(uuid, hypixelBowStatsToLeaderboards stats)]
      pure acc
  newacc <- liftMaybe somethingWentWrongMessage =<< createNewBowBotAccount (head $ mcNames mc) did uuid
  gid <- hInfoDB discordGuildIdInfo
  gmems <- discordGuildMembers gid
  for_ gmems $ \gmem -> when (maybe 0 userId (memberUser gmem) == did) $ updateRoles gmem (Just newacc)
  lift $ hRespond "*Registered successfully*"

registerCommand :: Command
registerCommand = Command CommandInfo
  { commandName = "register"
  , commandHelpEntries = [HelpEntry { helpUsage = "register [name]", helpDescription = "register your Minecraft name in Bow Bot", helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 30
  } $ hOneArgument' $ \name ->
    lift (hEnv envSender) >>=
      registerCommandBody RegisterCommandMessages {
          registerAlreadyBelongsMessage = "*That account already belongs to you!*",
          registerAlreadyBelongsSomeoneElseMessage = "*That account already belongs to someone else!*",
          registerAlreadyRegisteredMessage = "*You are already registered!*"
        } name . userId

getAndValidateDiscordId :: String -> ExceptT String CommandHandler UserId
getAndValidateDiscordId (readMaybe -> Just did) = do
  void $ liftMaybe "*The discord id doesn't exist!*" =<< getFromCache (Proxy @DiscordAccount) did
  return did
getAndValidateDiscordId (fromPingDiscordUser -> Just did) = do
  void $ liftMaybe "*The discord id doesn't exist!*" =<< getFromCache (Proxy @DiscordAccount) did
  return did
getAndValidateDiscordId _ = throwError "*The discord id is invalid!*"

addCommand :: Command
addCommand = Command CommandInfo
  { commandName = "add"
  , commandHelpEntries = [HelpEntry { helpUsage = "add [discord] [name]", helpDescription = "register someone in Bow Bot", helpGroup = "normal" }]
  , commandPerms = ModLevel
  , commandTimeout = 30
  } $ hTwoArguments' $ \did name ->
    getAndValidateDiscordId did >>=
      registerCommandBody RegisterCommandMessages {
          registerAlreadyBelongsMessage = "*That account already belongs to this user!*",
          registerAlreadyBelongsSomeoneElseMessage = "*That account already belongs to someone else!*",
          registerAlreadyRegisteredMessage = "*That person is not registered!*"
        } name

addaltCommand :: Command
addaltCommand = Command CommandInfo
  { commandName = "addalt"
  , commandHelpEntries = [HelpEntry { helpUsage = "addalt [discord] [name]", helpDescription = "register someone's alt account in Bow Bot", helpGroup = "normal" }]
  , commandPerms = ModLevel
  , commandTimeout = 30
  } $ hTwoArguments' $ \did name -> do
    bacc <- liftMaybe thePlayerIsntRegisteredMessage =<< getBowBotAccountByDiscord =<< getAndValidateDiscordId did
    uuid <- liftMaybe thePlayerDoesNotExistMessage =<< mcNameToUUID name
    baccother <- getBowBotAccountByMinecraft uuid
    for_ baccother $ \acc -> throwError $ if bacc == acc then "*That account already belongs to this user!*" else "*That account already belongs to someone else!*"
    cv <- tryIncreaseCounter (Proxy @HypixelApi) 1
    stats <- case cv of
      Nothing -> liftMaybe "*The player has never joined Hypixel!*" =<< requestHypixelBowStats uuid
      Just sec -> throwError $ "*Too many requests! Wait another " ++ show sec ++ " seconds!*"
    saved <- getFromCache (Proxy @MinecraftAccount) uuid
    case saved of -- TODO: remove repetition!
      Nothing -> do
        names <- liftMaybe thePlayerDoesNotExistMessage =<< mcUUIDToNames uuid
        let newacc = MinecraftAccount { mcUUID = uuid, mcNames = names, mcHypixelBow = NotBanned, mcHypixelWatchlist = False }
        a <- storeInCache [newacc]
        unless a $ throwError somethingWentWrongMessage
        when a $ void $ storeInCacheIndexed [(uuid, hypixelBowStatsToLeaderboards stats)]
      Just MinecraftAccount { mcHypixelBow = NotBanned } -> void $ storeInCacheIndexed [(uuid, hypixelBowStatsToLeaderboards stats)]
      _ -> pure ()
    newacc <- liftMaybe somethingWentWrongMessage =<< addAltToBowBotAccount (BowBot.Account.Basic.accountId bacc) uuid
    gid <- hInfoDB discordGuildIdInfo
    gmems <- discordGuildMembers gid
    for_ gmems $ \gmem -> when (maybe 0 userId (memberUser gmem) `elem` accountDiscords bacc) $ updateRoles gmem (Just newacc)
    lift $ hRespond "*Registered successfully*"