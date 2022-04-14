{-# LANGUAGE RecordWildCards #-}

module BowBot.Hypixel.StatsCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Minecraft.Arg
import BowBot.Hypixel.Stats
import BowBot.Settings.Basic (defSettings)


hypixelStatsCommand :: Command (Only MinecraftAccountArg) (Only MinecraftAccount)
hypixelStatsCommand = Command (Only (MinecraftAccountArg "name")) CommandInfo
  { commandName = "s"
  , commandDescription = "" -- TODO
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ withArgs $ \(Only MinecraftAccount {..}) -> do
    stats' <- requestHypixelBowStats mcUUID
    case stats' of
      Nothing -> hRespond "*The player has never joined Hypixel!*"
      Just stats -> hRespond $ "**" ++ head mcNames ++ "**:\n" ++ showHypixelBowStats defSettings stats
      -- TODO: update leaderboards
      -- TODO: save if enough wins