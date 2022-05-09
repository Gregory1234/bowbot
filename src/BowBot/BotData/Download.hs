{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module BowBot.BotData.Download where

import BowBot.BotData.Basic
import BowBot.BotData.Info
import BowBot.Minecraft.Account
import BowBot.Account.Basic
import Control.Concurrent.STM (STM, atomically)
import Database.MySQL.Simple (Connection)
import BowBot.BotData.Cached
import BowBot.BotData.CachedSingle
import BowBot.DB.Basic (withDB)
import BowBot.Command.Basic (PermissionLevel)
import BowBot.BotData.Counter
import BowBot.Hypixel.Basic
import BowBot.Settings.Basic
import Network.HTTP.Conduit (Manager)
import BowBot.Hypixel.Leaderboard
import BowBot.Discord.Roles
import BowBot.Hypixel.Guild
import BowBot.Discord.Account
import Discord
import Control.Monad.Reader
import BowBot.Hypixel.TimeStats
import BowBot.Hypixel.Watchlist
import Control.Concurrent.Async (concurrently_)
import BowBot.Birthday.Basic
import BowBot.Snipe.Basic


emptyBotData :: STM BotData
emptyBotData = do
  infoFieldCache <- newCache
  minecraftAccountCache <- newCache
  permissionCache <- newCache
  bowBotAccountCache <- newCache
  hypixelApiCounter <- newCounter
  settingsCache <- newCache
  hypixelLeaderboardCache <- newCache
  savedRolesCache <- newCache
  hypixelGuildMembersCache <- newCachedData
  discordAccountsCache <- newCache
  hypixelDailyStatsCache <- newCache
  hypixelWeeklyStatsCache <- newCache
  hypixelMonthlyStatsCache <- newCache
  hypixelOnlinePlayersCache <- newCachedData
  birthdayCache <- newCache
  snipeCache <- newCache
  return BotData {..}

refreshBotData :: Connection -> BotData -> IO ()
refreshBotData conn = runReaderT $ do
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
  map (`runReaderT` (dh, (manager, bdt)))
    [ updateMinecraftAccountCache
    , updateDiscordAccountCache
    , do
      updateHypixelLeaderboardCache
      forM_ times updateHypixelTimeStatsCache'
    ]

clearBotDataCaches :: BotData -> IO ()
clearBotDataCaches = runReaderT $ do
  clearCounter HypixelApi
  clearCacheSingle @HypixelGuildMembers
  clearCacheSingle @HypixelOnlinePlayers

downloadBotData :: IO BotData
downloadBotData = do
  bdt <- atomically emptyBotData
  withDB $ \conn -> refreshBotData conn bdt
  return bdt