{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module BowBot.Hypixel.StatsCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Minecraft.Arg
import BowBot.Hypixel.Stats
import BowBot.Settings.Basic
import BowBot.Utils (liftMaybe)
import BowBot.Hypixel.Basic (HypixelApi)
import Data.Proxy
import BowBot.BotData.Counter
import Control.Monad.Error.Class (throwError)
import Discord.Types (userId)


hypixelStatsCommand :: SettingsSource -> String -> Command (Only (MinecraftArg HypixelBowStats)) (Only (MinecraftResponse HypixelBowStats))
hypixelStatsCommand src name = Command (Only (MinecraftArg "name" helper)) CommandInfo
  { commandName = name
  , commandDescription = "" -- TODO
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ withArgs $ \(Only MinecraftResponse {responseAccount = MinecraftAccount {..}, ..}) -> do
    let (didYouMean, renderedName) = case responseType of
          JustResponse -> ("", head mcNames)
          OldResponse o -> ("", o ++ " (" ++ head mcNames ++ ")")
          DidYouMeanResponse -> ("*Did you mean* ", head mcNames)
          DidYouMeanOldResponse o -> ("*Did you mean* ", o ++ " (" ++ head mcNames ++ ")")
    user <- hEnv envSender
    settings <- getSettingsFromSource src (userId user)
    hRespond $ didYouMean ++ "**" ++ renderedName ++ "**:\n" ++ showHypixelBowStats settings responseValue
    -- TODO: update leaderboards
    -- TODO: save if enough wins
  where
    helper MinecraftAccount {..} = do
      cv <- tryIncreaseCounter (Proxy @HypixelApi) 1
      case cv of
        Nothing -> do
          stats <- liftMaybe "*The player has never joined Hypixel!*" =<< requestHypixelBowStats mcUUID
          return (bowWins stats + bowLosses stats /= 0, stats)
        Just sec -> throwError $ "*Too many requests! Wait another " ++ show sec ++ " seconds!*"