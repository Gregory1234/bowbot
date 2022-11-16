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
import BowBot.Counter.Basic
import BowBot.Discord.Roles
import BowBot.Hypixel.Leaderboard (hypixelBowStatsToLeaderboards)
import BowBot.BotData.Info
import BowBot.Discord.Utils
import BowBot.Account.Basic
import BowBot.Command.Tips


hypixelTimeStatsCommand :: SettingsSource -> String -> String -> Command
hypixelTimeStatsCommand src name desc = Command CommandInfo
  { commandName = name
  , commandHelpEntries = [HelpEntry { helpUsage = name ++ " [name]", helpDescription = desc, helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument (minecraftArgFullConstraintWithSkipTip helper) $ \(MinecraftResponse {mcResponseAccount = mcResponseAccount@MinecraftAccount {..}, ..}, stats) -> do
    let (didYouMean, renderedName) = (if mcResponseAutocorrect == ResponseAutocorrect then "*Did you mean* " else "", showMinecraftAccountDiscord mcResponseTime mcResponseAccount)
    user <- envs envSender
    settings <- getSettingsFromSource src (userId user)
    dailyStats <- getFromCache @(HypixelBowTimeStats 'DailyStats) mcUUID
    weeklyStats <- getFromCache @(HypixelBowTimeStats 'WeeklyStats) mcUUID
    monthlyStats <- getFromCache @(HypixelBowTimeStats 'MonthlyStats) mcUUID
    when (bowWins stats >= 50 && mcResponseAutocorrect == ResponseNew) $ do -- TODO: remove repetition?
      minecraftNewAccountTip mcResponseAccount
    respond $ didYouMean ++ renderedName ++ ":\n" ++ showMaybeHypixelBowTimeStats settings stats dailyStats ++ "\n" ++ showMaybeHypixelBowTimeStats settings stats weeklyStats ++ "\n" ++ showMaybeHypixelBowTimeStats settings stats monthlyStats
    when (bowWins stats >= 50 && mcResponseAutocorrect == ResponseNew) $ do
      a <- storeInCache [mcResponseAccount]
      when a $ void $ storeInCacheIndexed [(mcUUID, hypixelBowStatsToLeaderboards stats)]
    when (mcResponseAutocorrect == ResponseTrue) $ void $ storeInCacheIndexed [(mcUUID, hypixelBowStatsToLeaderboards stats)]
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
          return (if bowWins stats + bowLosses stats /= 0 then ResponseGood else ResponseFindBetter, stats)
        Just sec -> throwError $ "*Too many requests! Wait another " ++ show sec ++ " seconds!*"