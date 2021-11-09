{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module BowBot.Command.Ban where

import BowBot.Command
import BowBot.Minecraft
import BowBot.Stats

banCommand :: StatType s => Proxy s -> String -> (MinecraftAccount -> MinecraftAccount) -> Command
banCommand p name f = Command name ModLevel 10 $ \m man bdt -> do
  uuid' <- liftIO $ mcNameToUUID man bdt (words (unpack (messageText m)) !! 1)
  case uuid' of
    Nothing -> respond m "Player not found! For safety reasons this command does not have autocorrect enabled."
    Just uuid -> do
      r <- liftIO $ banLeaderboard p man uuid
      case r of
        Just True -> do
          liftIO $ atomically $ modifyTVar (minecraftAccounts bdt) $ map $ \mc@MinecraftAccount {..} -> if mcUUID == uuid then f mc else mc
          respond m "Success, player got banned!"
        Just False -> respond m "Player not found! For safety reasons this command does not have autocorrect enabled."
        Nothing -> respond m somethingWrongMessage