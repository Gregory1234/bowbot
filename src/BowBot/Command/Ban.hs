{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module BowBot.Command.Ban where

import BowBot.Command
import BowBot.Minecraft
import BowBot.Stats.HypixelBow

hypixelBowLeaderboardBanCommand :: String -> Command
hypixelBowLeaderboardBanCommand name = Command name ModLevel 10 $ do
  bdt <- hData
  pName <- hArg 1
  uuid' <- mcNameToUUID bdt (fromMaybe "" pName)
  case uuid' of
    Nothing -> hRespond "Player not found! For safety reasons this command does not have autocorrect enabled."
    Just uuid -> do
      r <- banHypixelBowLeaderboard uuid
      case r of
        Just True -> do
          hModify minecraftAccounts $ map $ \mc@MinecraftAccount {..} -> if mcUUID == uuid then mc { mcHypixelBow = Banned } else mc
          hRespond "Success, player got banned!"
        Just False -> hRespond "Player not found! For safety reasons this command does not have autocorrect enabled."
        Nothing -> hRespond somethingWrongMessage