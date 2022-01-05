{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module BowBot.Command.Ban where

import BowBot.Command
import BowBot.Minecraft
import BowBot.Stats.HypixelBow

hypixelBowLeaderboardBanCommand :: String -> Command
hypixelBowLeaderboardBanCommand name = Command name ModLevel 10 $ do
  pName <- hArg 1
  uuid' <- mcNameToUUID (fromMaybe "" pName)
  case uuid' of
    Nothing -> hRespond "Player not found! For safety reasons this command does not have autocorrect enabled."
    Just uuid -> do
      r <- banHypixelBowLeaderboard uuid
      if r 
      then do
        hModify minecraftAccounts $ map $ \mc@MinecraftAccount {..} -> if mcUUID == uuid then mc { mcHypixelBow = Banned } else mc
        hRespond "Success, player got banned!"
      else
        hRespond "Player not found! For safety reasons this command does not have autocorrect enabled."