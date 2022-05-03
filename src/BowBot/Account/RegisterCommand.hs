{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module BowBot.Account.RegisterCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Account.Basic
import BowBot.Minecraft.Arg
import BowBot.Discord.Utils
import Control.Monad.Trans (lift)
import Control.Monad.Error.Class (throwError)
import BowBot.BotData.Cached (storeInCache, getFromCache, storeInCacheIndexed)
import BowBot.Hypixel.Basic (HypixelApi)
import BowBot.Hypixel.Leaderboard (hypixelBowStatsToLeaderboards)
import Data.Data
import BowBot.BotData.Counter (tryIncreaseCounter)
import BowBot.Account.Register
import BowBot.Discord.Roles (updateRoles)
import BowBot.BotData.Info (hInfoDB, discordGuildIdInfo)
import BowBot.Hypixel.Stats (requestHypixelBowStats)


registerCommand :: Command
registerCommand = Command CommandInfo
  { commandName = "register"
  , commandUsage = "register [name]"
  , commandDescription = "register your Minecraft name in Bow Bot"
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  , commandGroup = "normal"
  } $ hOneArgument' $ \name -> do
    uuid <- liftMaybe thePlayerDoesNotExistMessage =<< mcNameToUUID name
    sender <- userId <$> lift (hEnv envSender)
    bacc <- getBowBotAccountByMinecraft uuid
    for_ bacc $ \BowBotAccount {..} -> throwError $ if sender `elem` accountDiscords then "*That account already belongs to you!*" else "*That account already belongs to someone else!*"
    baccdc <- getBowBotAccountByDiscord sender
    for_ baccdc $ \_ -> throwError "*You are already registered!*"
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
        void $ storeInCacheIndexed [(uuid, hypixelBowStatsToLeaderboards stats)]
        pure acc
    newacc <- liftMaybe somethingWentWrongMessage =<< createNewBowBotAccount (head $ mcNames mc) sender uuid
    gid <- hInfoDB discordGuildIdInfo
    gmems <- discordGuildMembers gid
    for_ gmems $ \gmem -> when (maybe 0 userId (memberUser gmem) == sender) $ updateRoles gmem (Just newacc)
    lift $ hRespond "*Registered successfully*"
