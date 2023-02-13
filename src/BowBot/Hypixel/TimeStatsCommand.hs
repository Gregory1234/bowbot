{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

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
import BowBot.Hypixel.Leaderboard
import BowBot.Hypixel.Announce
import BowBot.BotData.Info
import BowBot.Discord.Utils
import BowBot.Account.Basic
import BowBot.Command.Tips


hypixelTimeStatsCommand :: SettingsSource -> Text -> Text -> Command
hypixelTimeStatsCommand src name desc = Command CommandInfo
  { commandName = name
  , commandHelpEntries = [HelpEntry { helpUsage = name <> " [name]", helpDescription = desc, helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument (minecraftArgFullConstraintWithSkipTip helper) $ \(MinecraftResponse {mcResponseAccount = mcResponseAccount@MinecraftAccount {..}, ..}, stats) -> do
    let (didYouMean, renderedName) = (if mcResponseAutocorrect == ResponseAutocorrect then "*Did you mean* " else "", showMinecraftAccountDiscord mcResponseTime mcResponseAccount)
    user <- envs envSender
    settings <- getSettingsFromSource src (userId user)
    dailyStats <- getFromCache @(HypixelBowTimeStats 'DailyStats) mcUUID
    weeklyStats <- getFromCache @(HypixelBowTimeStats 'WeeklyStats) mcUUID
    monthlyStats <- getFromCache @(HypixelBowTimeStats 'MonthlyStats) mcUUID
    let addAccount = bowWins stats >= 50 && mcResponseAutocorrect == ResponseNew
    when addAccount $ minecraftNewAccountTip mcResponseAccount
    respond $ didYouMean <> renderedName <> ":\n" <> showMaybeHypixelBowTimeStats settings stats dailyStats <> "\n" <> showMaybeHypixelBowTimeStats settings stats weeklyStats <> "\n" <> showMaybeHypixelBowTimeStats settings stats monthlyStats
    when addAccount $ do
      a <- storeInCache [mcResponseAccount]
      when a $ void $ setHypixelBowLeaderboardEntryByUUID mcUUID (hypixelBowStatsToLeaderboards stats)
    when (mcResponseAutocorrect /= ResponseNew) $ do
      void $ setHypixelBowLeaderboardEntryByUUID mcUUID (hypixelBowStatsToLeaderboards stats)
      updateRolesDivisionTitleByUUID mcUUID
      announceMilestones
  where
    helper MinecraftAccount {..} = do
      cv <- tryIncreaseCounter HypixelApi 1
      case cv of
        Nothing -> do
          stats <- liftMaybe "*The player has never joined Hypixel!*" =<< requestHypixelBowStats mcUUID
          return (if bowWins stats + bowLosses stats /= 0 then ResponseGood else ResponseFindBetter, stats)
        Just sec -> throwError $ "*Too many requests! Wait another " <> showt sec <> " seconds!*"