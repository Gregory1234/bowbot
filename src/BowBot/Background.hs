{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module BowBot.Background where

import Discord
import qualified Discord.Requests as R
import BowBot.BotData
import Control.Concurrent.STM (atomically, readTVar)
import Control.Monad (when)
import BowBot.Stats.HypixelBow
import Data.Proxy
import BowBot.Stats
import Network.HTTP.Conduit (newManager, Manager)
import BowBot.API
import Data.List.Split (chunksOf)
import Data.Traversable (for)
import Control.Concurrent (threadDelay)
import Data.Map (fromList)
import Control.Concurrent.Async (mapConcurrently)
import Data.Foldable (for_)
import Data.Maybe (catMaybes, fromMaybe)
import BowBot.Utils
import Discord.Types
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Types (object, (.=), parseMaybe, (.:))
import Data.Text (pack)
import Data.Either (fromRight)
import Text.Read (readMaybe)
import Data.Aeson (decode)

getDiscordIds :: Manager -> IO [UserId]
getDiscordIds manager = do
  res <- sendDB manager "discord/all.php" []
  let parser = parseMaybe $ \o -> do
        dt <- o .: "data"
        for dt $ \s -> do
          (readMaybe -> Just x) <- pure s
          return x
  return . fromMaybe [] $ decode res >>= parser

addDiscords :: DiscordHandler ()
addDiscords = do
  manager <- liftIO $ newManager managerSettings
  uids <- liftIO $ getDiscordIds manager
  dgid <- liftIO discordGuildId
  v <- fmap (filter (not . userIsBot . memberUser)) <$> restCall (R.ListGuildMembers dgid R.GuildMembersTiming {R.guildMembersTimingLimit = Just 500, R.guildMembersTimingAfter = Nothing})
  case v of
    Right x -> do
      let uids' = filter (\u -> all (\m -> userId (memberUser m) /= u) x) uids
      y <- traverse helper uids'
      liftIO $ updateDiscords manager x y
    Left x -> liftIO $ print x
  where
    helper :: UserId -> DiscordHandler User
    helper u = do
      y <- restCall (R.GetUser u)
      return $ fromRight undefined y
    updateDiscords manager mem usr = sendPostDB manager "discord/update.php" (object $ map memToObject mem ++ map usrToObject usr)
       where
         memToObject GuildMember {memberUser = memberUser@User {..}, ..} = case memberNick of
           Nothing -> usrToObject memberUser
           Just nick -> pack (show userId) .= object ["name" .= userName, "discriminator" .= userDiscrim, "nickname" .= nick]
         usrToObject User {..} = pack (show userId) .= object ["name" .= userName, "discriminator" .= userDiscrim]

discordBackgroundMinutely :: BotData -> Int -> DiscordHandler ()
discordBackgroundMinutely _ mint = do
  when (mint == 0) $ do
    addDiscords

-- TODO: frequency updates

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

backgroundMinutely :: BotData -> Int -> IO ()
backgroundMinutely bdt@BotData {..} mint = do
  atomically $ clearApiRequestCounter hypixelRequestCounter
  when (mint == 0) $ do
    downloadData bdt
    manager <- newManager managerSettings
    updateMinecraftAccounts bdt manager
  when (mint == 30) $ do
    hour <- read @Int <$> getTime "%k"
    when (even hour) $
      completeLeaderboardUpdate (Proxy @HypixelBowStats) bdt hypixelRequestCounter $ \MinecraftAccount {..} -> mcHypixelBow == BiHourly
    when (hour == 0) $
      completeLeaderboardUpdate (Proxy @HypixelBowStats) bdt hypixelRequestCounter $ \MinecraftAccount {..} -> mcHypixelBow == Daily