{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module BowBot.Background where

import Discord
import BowBot.BotData
import Control.Concurrent.STM (atomically, readTVar)
import Control.Monad (when)
import BowBot.Stats.HypixelBow
import Data.Proxy
import BowBot.Stats
import Network.HTTP.Conduit (newManager)
import BowBot.API
import Data.List.Split (chunksOf)
import Data.Traversable (for)
import Control.Concurrent (threadDelay)
import Data.Map (fromList)
import Control.Concurrent.Async (mapConcurrently)
import Data.Foldable (for_)
import Data.Maybe (catMaybes)

completeLeaderboardUpdate :: StatType s => Proxy s -> BotData -> ApiRequestCounter -> (MinecraftAccount -> Bool) -> IO ()
completeLeaderboardUpdate pr bdt api filt = do
  manager <- newManager managerSettings
  mcs <- atomically $ readTVar $ minecraftAccounts bdt
  let chunked = chunksOf 25 (map mcUUID $ filter filt mcs)
  for_ chunked $ helper manager
    where
      helper manager lst = do
        tryApiRequests api 25 (\x -> do { threadDelay ((x+10) * 1000000); helper manager lst }) $ do
          let chunked = chunksOf 10 lst
          dt <- fmap (fromList . zip lst . catMaybes . concat) $ for chunked $ mapConcurrently $ fmap (fmap toLeaderboard) . requestStats pr manager
          updateLeaderboard manager dt
          

discordBackgroundMinutely :: BotData -> Int -> DiscordHandler ()
discordBackgroundMinutely _ _ = pure ()

backgroundMinutely :: BotData -> Int -> IO ()
backgroundMinutely bdt@BotData {..} mint = do
  atomically $ clearApiRequestCounter hypixelRequestCounter
  when (mint == 0) $ do
    downloadData bdt
    manager <- newManager managerSettings
    updateMinecraftAccounts bdt manager
  when (mint == 30) $ completeLeaderboardUpdate (Proxy @HypixelBowStats) bdt hypixelRequestCounter $ \MinecraftAccount {..} -> True -- TODO: different frequencies