module BowBot.Hypixel.StatsCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Hypixel.Stats
import BowBot.Settings.Basic (defSettings)


hypixelStatsCommand :: Command (Only SingleStringArg) (Only String)
hypixelStatsCommand = Command (Only (SingleStringArg "name")) CommandInfo
  { commandName = "s"
  , commandDescription = "" -- TODO
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ withArgs $ \(Only name) -> do
    uuid' <- mcNameToUUID name
    case uuid' of
      Nothing -> hRespond "*The player doesn't exist!*"
      Just uuid -> do
        -- TODO rate limits
        stats' <- requestHypixelBowStats uuid
        case stats' of
          Nothing -> hRespond "*The player has never joined Hypixel!*"
          Just stats -> hRespond $ "**" ++ name ++ "**:\n" ++ showHypixelBowStats defSettings stats
          -- TODO: get true name
          -- TODO: add autocorrect
          -- TODO: update leaderboards
          -- TODO: save if enough wins