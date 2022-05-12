{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.Hypixel.StatsCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Minecraft.Arg
import BowBot.Hypixel.Stats
import BowBot.Settings.Basic
import BowBot.Utils
import BowBot.Hypixel.Basic (HypixelApi(..))
import BowBot.BotData.Counter
import Control.Monad.Error.Class (throwError)
import Discord.Types
import BowBot.Hypixel.Leaderboard
import BowBot.BotData.Cached (storeInCacheIndexed, storeInCache)
import BowBot.BotData.Info
import BowBot.Discord.Roles
import BowBot.Account.Basic
import BowBot.Discord.Utils (discordGuildMembers)

hypixelStatsCommand :: SettingsSource -> String -> String -> Command
hypixelStatsCommand src name desc = Command CommandInfo
  { commandName = name
  , commandHelpEntries = [HelpEntry { helpUsage = name ++ " [name]", helpDescription = desc, helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument (\s -> lift (envs envSender) >>= flip (minecraftArgFullConstraint helper) s . userId) $ \(MinecraftResponse {mcResponseAccount = mcResponseAccount@MinecraftAccount {..}, ..}, stats) -> do
    let (didYouMean, renderedName) = (if mcResponseAutocorrect == ResponseAutocorrect then "*Did you mean* " else "", showMinecraftAccountDiscord mcResponseTime mcResponseAccount)
    user <- envs envSender
    settings <- getSettingsFromSource src (userId user)
    respond $ didYouMean ++ renderedName ++ ":\n" ++ showHypixelBowStats settings stats
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