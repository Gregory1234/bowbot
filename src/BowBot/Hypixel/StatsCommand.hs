{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module BowBot.Hypixel.StatsCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Minecraft.Arg
import BowBot.Hypixel.Stats
import BowBot.Settings.Basic
import BowBot.Utils
import BowBot.Hypixel.Basic (HypixelApi)
import Data.Proxy
import BowBot.BotData.Counter
import Control.Monad.Error.Class (throwError)
import Discord.Types
import BowBot.Hypixel.Leaderboard
import BowBot.BotData.Cached (storeInCacheIndexed, getFromCache, storeInCache)
import BowBot.BotData.Info
import BowBot.Discord.Roles
import BowBot.Account.Basic
import BowBot.Discord.Utils (discordGuildMembers)
import Control.Monad.Trans (lift)

hypixelStatsCommand :: SettingsSource -> String -> String -> Command
hypixelStatsCommand src name desc = Command CommandInfo
  { commandName = name
  , commandHelpEntries = [HelpEntry { helpUsage = name ++ " [name]", helpDescription = desc, helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ hOneOptionalArgument (\s -> lift (hEnv envSender) >>= minecraftArgDefault helper s . userId) $ \MinecraftResponse {responseAccount = responseAccount@MinecraftAccount {..}, ..} -> do
    let (didYouMean, renderedName) = case responseType of
          JustResponse -> ("", head mcNames)
          OldResponse o -> ("", o ++ " (" ++ head mcNames ++ ")")
          DidYouMeanResponse -> ("*Did you mean* ", head mcNames)
          DidYouMeanOldResponse o -> ("*Did you mean* ", o ++ " (" ++ head mcNames ++ ")")
    user <- hEnv envSender
    settings <- getSettingsFromSource src (userId user)
    hRespond $ didYouMean ++ "**" ++ renderedName ++ "**:\n" ++ showHypixelBowStats settings responseValue
    saved <- getFromCache (Proxy @MinecraftAccount) mcUUID
    case saved of
      Nothing | bowWins responseValue >= 50 -> do
        a <- storeInCache [responseAccount]
        when a $ void $ storeInCacheIndexed [(mcUUID, hypixelBowStatsToLeaderboards responseValue)]
      Just MinecraftAccount { mcHypixelBow = NotBanned } -> do
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