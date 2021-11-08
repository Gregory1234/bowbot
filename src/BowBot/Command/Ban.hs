{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module BowBot.Command.Ban where

import BowBot.Command
import BowBot.API
import BowBot.Minecraft

-- TODO: generalize
banCommand :: String -> Command
banCommand name = Command name ModLevel 10 $ \m man bdt -> do
  uuid' <- liftIO $ mcNameToUUID man bdt (words (unpack (messageText m)) !! 1)
  case uuid' of
    Nothing -> respond m "Player not found! For safety reasons this command does not have autocorrect enabled."
    Just uuid -> do
      res <- liftIO $ sendDB man "stats/hypixel/ban.php" ["uuid="++uuid]
      r <- liftIO $ decodeParse res $ \o -> o .: "success"
      case r of
        Just True -> do
          liftIO $ atomically $ modifyTVar (minecraftAccounts bdt) $ map $ \mc@MinecraftAccount {..} -> if mcUUID == uuid then mc { mcHypixelBow = Banned } else mc
          respond m "Success, player got banned!"
        Just False -> respond m "Player not found! For safety reasons this command does not have autocorrect enabled."
        Nothing -> respond m somethingWrongMessage