{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module BowBot.BotData.Download where

import BowBot.BotData.Basic
import BowBot.BotData.Info
import BowBot.Minecraft.Account
import BowBot.Account.Basic
import Control.Concurrent.STM (STM, newTVar, atomically)
import Data.HashMap.Strict (empty)
import Database.MySQL.Simple (Connection)
import BowBot.BotData.Cached
import BowBot.BotData.CachedSingle
import BowBot.DB.Basic (withDB)
import BowBot.Command.Basic (PermissionLevel)
import BowBot.BotData.Counter
import BowBot.Hypixel.Basic
import BowBot.Settings.Basic
import Network.HTTP.Conduit (Manager)
import BowBot.Network.Monad (runNetworkT)
import BowBot.Hypixel.Leaderboard
import BowBot.Discord.Roles
import BowBot.Hypixel.Guild
import BowBot.Discord.Account
import Discord
import BowBot.Discord.Monad
import Control.Monad.Reader
import BowBot.Hypixel.TimeStats
import BowBot.Hypixel.Watchlist
import Control.Concurrent.Async (concurrently_)
import BowBot.Birthday.Basic
import BowBot.Snipe.Basic


emptyBotData :: STM BotData
emptyBotData = do
  infoFieldCache <- newTVar empty
  minecraftAccountCache <- newTVar empty
  permissionCache <- newTVar empty
  bowBotAccountCache <- newTVar empty
  hypixelApiCounter <- newCounter
  settingsCache <- newTVar empty
  hypixelLeaderboardCache <- newTVar empty
  savedRolesCache <- newTVar empty
  hypixelGuildMembersCache <- newCachedData
  discordAccountsCache <- newTVar empty
  hypixelDailyStatsCache <- newTVar empty
  hypixelWeeklyStatsCache <- newTVar empty
  hypixelMonthlyStatsCache <- newTVar empty
  hypixelOnlinePlayersCache <- newCachedData
  birthdayCache <- newTVar empty
  snipeCache <- newTVar empty
  return BotData {..}

refreshBotData :: Connection -> BotData -> IO ()
refreshBotData conn bdt = flip runBotDataT bdt $ do
  refreshCache @InfoField conn
  refreshCache @MinecraftAccount conn
  refreshCache @PermissionLevel conn
  refreshCache @BowBotAccount conn
  refreshCache @Settings conn
  refreshCache @HypixelBowLeaderboardEntry conn
  refreshCache @SavedRoles conn
  refreshCache @DiscordAccount conn
  refreshCache @(HypixelBowTimeStats 'DailyStats) conn
  refreshCache @(HypixelBowTimeStats 'WeeklyStats) conn
  refreshCache @(HypixelBowTimeStats 'MonthlyStats) conn
  refreshCache @BirthdayDate conn
  refreshCache @SnipeMessage conn -- TODO: this is meaningless...

updateBotData :: [StatsTimeRange] -> Manager -> BotData -> DiscordHandler ()
updateBotData times manager bdt = ReaderT $ \dh -> foldl1 concurrently_ $
  map (flip runDiscordHandlerT dh . flip runNetworkT manager . flip runBotDataT bdt)
    [ updateCache @MinecraftAccount
    , updateCache @DiscordAccount
    , do
      updateCache @HypixelBowLeaderboardEntry
      when (DailyStats `elem` times) $ updateCache @(HypixelBowTimeStats 'DailyStats)
      when (WeeklyStats `elem` times) $ updateCache @(HypixelBowTimeStats 'WeeklyStats)
      when (MonthlyStats `elem` times) $ updateCache @(HypixelBowTimeStats 'MonthlyStats)
    ]

clearBotDataCaches :: BotData -> IO ()
clearBotDataCaches bdt = flip runBotDataT bdt $ do
  clearCounter HypixelApi
  clearCacheSingle @HypixelGuildMembers
  clearCacheSingle @HypixelOnlinePlayers

downloadBotData :: IO BotData
downloadBotData = do
  bdt <- atomically emptyBotData
  withDB $ \conn -> refreshBotData conn bdt
  return bdt