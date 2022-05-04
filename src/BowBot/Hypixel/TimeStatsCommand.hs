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
import Data.Proxy
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
  } $ hOneOptionalArgument (\s -> lift (hEnv envSender) >>= minecraftArgDefault helper s . userId) $ \MinecraftResponse {responseAccount = responseAccount@MinecraftAccount {..}, ..} -> do
    let (didYouMean, renderedName) = case responseType of
          JustResponse -> ("", "**" ++ head mcNames ++ "**")
          OldResponse o -> ("", "**" ++ o ++ "** (" ++ head mcNames ++ ")")
          DidYouMeanResponse -> ("*Did you mean* ", "**" ++ head mcNames ++ "**")
          DidYouMeanOldResponse o -> ("*Did you mean* ", "**" ++ o ++ "** (" ++ head mcNames ++ ")")
    user <- hEnv envSender
    settings <- getSettingsFromSource src (userId user)
    dailyStats <- getFromCache (Proxy @(HypixelBowTimeStats 'DailyStats)) mcUUID
    weeklyStats <- getFromCache (Proxy @(HypixelBowTimeStats 'WeeklyStats)) mcUUID
    monthlyStats <- getFromCache (Proxy @(HypixelBowTimeStats 'MonthlyStats)) mcUUID
    hRespond $ didYouMean ++ renderedName ++ ":\n" ++ showMaybeHypixelBowTimeStats settings responseValue dailyStats ++ "\n" ++ showMaybeHypixelBowTimeStats settings responseValue weeklyStats ++ "\n" ++ showMaybeHypixelBowTimeStats settings responseValue monthlyStats
    saved <- getFromCache (Proxy @MinecraftAccount) mcUUID
    case saved of
      Nothing | bowWins responseValue >= 50 -> do
        a <- storeInCache [responseAccount]
        when a $ void $ storeInCacheIndexed [(mcUUID, hypixelBowStatsToLeaderboards responseValue)]
      Just MinecraftAccount { mcHypixelBow = NotBanned} -> do
        void $ storeInCacheIndexed [(mcUUID, hypixelBowStatsToLeaderboards responseValue)]
      _ -> pure ()
    gid <- hInfoDB discordGuildIdInfo
    gmems <- discordGuildMembers gid
    acc' <- getBowBotAccountByMinecraft mcUUID
    for_ acc' $ \acc -> for_ gmems $ \gmem -> when (maybe 0 userId (memberUser gmem) `elem` accountDiscords acc) $ updateRolesDivisionTitle gmem (Just acc)
  where
    helper MinecraftAccount {..} = do
      cv <- tryIncreaseCounter (Proxy @HypixelApi) 1
      case cv of
        Nothing -> do
          stats <- liftMaybe "*The player has never joined Hypixel!*" =<< requestHypixelBowStats mcUUID
          return (bowWins stats + bowLosses stats /= 0, stats)
        Just sec -> throwError $ "*Too many requests! Wait another " ++ show sec ++ " seconds!*"