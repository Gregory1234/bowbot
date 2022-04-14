{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.Hypixel.StatsCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Minecraft.Arg
import BowBot.Hypixel.Stats
import BowBot.Settings.Basic (defSettings)
import BowBot.Utils (liftMaybe)


hypixelStatsCommand :: Command (Only (MinecraftArg HypixelBowStats)) (Only (MinecraftResponse HypixelBowStats))
hypixelStatsCommand = Command (Only (MinecraftArg "name" helper)) CommandInfo
  { commandName = "s"
  , commandDescription = "" -- TODO
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ withArgs $ \(Only MinecraftResponse {responseAccount = MinecraftAccount {..}, ..}) -> do
    let (didYouMean, renderedName) = case responseType of
          JustResponse -> ("", head mcNames)
          OldResponse o -> ("", o ++ " (" ++ head mcNames ++ ")")
          DidYouMeanResponse -> ("*Did you mean* ", head mcNames)
          DidYouMeanOldResponse o -> ("*Did you mean* ", o ++ " (" ++ head mcNames ++ ")")
    hRespond $ didYouMean ++ "**" ++ renderedName ++ "**:\n" ++ showHypixelBowStats defSettings responseValue
    -- TODO: update leaderboards
    -- TODO: save if enough wins
  where
    helper MinecraftAccount {..} = liftMaybe "*The player has never joined Hypixel!*" =<< requestHypixelBowStats mcUUID