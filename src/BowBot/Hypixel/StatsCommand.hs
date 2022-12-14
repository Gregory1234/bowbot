{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.Hypixel.StatsCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Minecraft.Arg
import BowBot.Hypixel.Stats
import BowBot.Settings.Basic
import BowBot.Utils
import BowBot.Hypixel.Basic (HypixelApi(..))
import BowBot.Counter.Basic
import Control.Monad.Error.Class (throwError)
import Discord.Types
import BowBot.Hypixel.Leaderboard
import BowBot.BotData.Cached (storeInCacheIndexed, storeInCache, getFromCache)
import BowBot.BotData.Info
import BowBot.Discord.Roles
import BowBot.Account.Basic
import BowBot.Discord.Utils (discordGuildMembers)
import BowBot.Command.Tips

hypixelStatsCommand :: SettingsSource -> Text -> Text -> Command
hypixelStatsCommand src name desc = Command CommandInfo
  { commandName = name
  , commandHelpEntries = [HelpEntry { helpUsage = name <> " [name]", helpDescription = desc, helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument (minecraftArgFullConstraintWithSkipTip helper) $ \(MinecraftResponse {mcResponseAccount = mcResponseAccount@MinecraftAccount {..}, ..}, stats) -> do
    let (didYouMean, renderedName) = (if mcResponseAutocorrect == ResponseAutocorrect then "*Did you mean* " else "", showMinecraftAccountDiscord mcResponseTime mcResponseAccount)
    user <- envs envSender
    settings <- getSettingsFromSource src (userId user)
    when (bowWins stats >= 50 && mcResponseAutocorrect == ResponseNew) $ do -- TODO: remove repetition?
      minecraftNewAccountTip mcResponseAccount
    respond $ didYouMean <> renderedName <> ":\n" <> showHypixelBowStats settings stats
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
          oldstats <- getFromCache mcUUID
          return (if bowWins stats + bowLosses stats /= 0 then ResponseGood else ResponseFindBetter, completeHypixelBowStats stats oldstats)
        Just sec -> throwError $ "*Too many requests! Wait another " <> showt sec <> " seconds!*"