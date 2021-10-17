{-# LANGUAGE RecordWildCards #-}

module BowBot.Background where

import Discord
import BowBot.BotData
import Control.Concurrent.STM (atomically)

discordBackgroundMinutely :: BotData -> Int -> DiscordHandler ()
discordBackgroundMinutely _ _ = pure ()

backgroundMinutely :: BotData -> Int -> IO ()
backgroundMinutely BotData {..} _ = do
  atomically $ clearApiRequestCounter hypixelRequestCounter