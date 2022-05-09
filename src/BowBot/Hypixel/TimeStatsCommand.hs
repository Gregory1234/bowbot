{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.Hypixel.TimeStatsCommand where

import BowBot.Command
import BowBot.Settings.Basic
import BowBot.Minecraft.Account
import BowBot.Minecraft.Arg
import BowBot.Hypixel.Basic
import BowBot.Hypixel.Stats
import BowBot.Hypixel.TimeStats
import BowBot.BotData.Cached
import Control.Monad.Error.Class (throwError)
import BowBot.BotData.Counter
import BowBot.Discord.Roles
import Control.Monad.Trans (lift)
import BowBot.Hypixel.Leaderboard (hypixelBowStatsToLeaderboards)
import BowBot.BotData.Info
import BowBot.Discord.Utils
import BowBot.Account.Basic


hypixelTimeStatsCommand :: SettingsSource -> String -> String -> Command
hypixelTimeStatsCommand src name desc = Command CommandInfo
  { commandName = name
  , commandHelpEntries = [HelpEntry { helpUsage = name ++ " [name]", helpDescription = desc, helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument (\s -> lift (envs envSender) >>= minecraftArgDefault helper s . userId) $ \MinecraftResponse {responseAccount = responseAccount@MinecraftAccount {..}, ..} -> do
    let (didYouMean, renderedName) = (if isDidYouMean responseType then "*Did you mean* " else "", showMinecraftAccountDiscord responseType responseAccount)
    user <- envs envSender
    settings <- getSettingsFromSource src (userId user)
    dailyStats <- getFromCache @(HypixelBowTimeStats 'DailyStats) mcUUID
    weeklyStats <- getFromCache @(HypixelBowTimeStats 'WeeklyStats) mcUUID
    monthlyStats <- getFromCache @(HypixelBowTimeStats 'MonthlyStats) mcUUID
    respond $ didYouMean ++ renderedName ++ ":\n" ++ showMaybeHypixelBowTimeStats settings responseValue dailyStats ++ "\n" ++ showMaybeHypixelBowTimeStats settings responseValue weeklyStats ++ "\n" ++ showMaybeHypixelBowTimeStats settings responseValue monthlyStats
    saved <- getFromCache mcUUID
    case saved of
      Nothing | bowWins responseValue >= 50 -> do
        a <- storeInCache [responseAccount]
        when a $ void $ storeInCacheIndexed [(mcUUID, hypixelBowStatsToLeaderboards responseValue)]
      Just MinecraftAccount { mcHypixelBow = NotBanned} -> do
        void $ storeInCacheIndexed [(mcUUID, hypixelBowStatsToLeaderboards responseValue)]
      _ -> pure ()
    gid <- askInfo discordGuildIdInfo
    gmems <- discordGuildMembers gid
    acc' <- getBowBotAccountByMinecraft mcUUID
    for_ acc' $ \acc -> for_ gmems $ \gmem -> when (maybe 0 userId (memberUser gmem) `elem` accountDiscords acc) $ updateRolesDivisionTitle gmem (Just acc)
  where
    helper MinecraftAccount {..} = do
      cv <- tryIncreaseCounter HypixelApi 1
      case cv of
        Nothing -> do
          stats <- liftMaybe "*The player has never joined Hypixel!*" =<< requestHypixelBowStats mcUUID
          return (bowWins stats + bowLosses stats /= 0, stats)
        Just sec -> throwError $ "*Too many requests! Wait another " ++ show sec ++ " seconds!*"