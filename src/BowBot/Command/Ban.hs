{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module BowBot.Command.Ban where

import BowBot.Command
import BowBot.Minecraft
import BowBot.Stats

banCommand :: StatType s => Proxy s -> String -> (MinecraftAccount -> MinecraftAccount) -> Command
banCommand p name f = Command name ModLevel 10 $ do
  man <- hManager
  bdt <- hData
  pName <- hArg 1
  uuid' <- liftIO $ mcNameToUUID man bdt (fromMaybe "" pName)
  case uuid' of
    Nothing -> hRespond "Player not found! For safety reasons this command does not have autocorrect enabled."
    Just uuid -> do
      r <- liftIO $ banLeaderboard p man uuid
      case r of
        Just True -> do
          hModify minecraftAccounts $ map $ \mc@MinecraftAccount {..} -> if mcUUID == uuid then f mc else mc
          hRespond "Success, player got banned!"
        Just False -> hRespond "Player not found! For safety reasons this command does not have autocorrect enabled."
        Nothing -> hRespond somethingWrongMessage