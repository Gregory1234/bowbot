{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BowBot.BotData.Download where

import BowBot.BotData.Basic
import BowBot.BotData.Info
import BowBot.Minecraft.Account
import BowBot.Account.Basic
import Database.MySQL.Simple (Connection)
import BowBot.BotData.Cached
import BowBot.BotData.CachedSingle
import BowBot.DB.Basic (withDB, queryLog)
import BowBot.Command.Basic (PermissionLevel)
import BowBot.Counter.Basic
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
import BowBot.Utils


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

refreshBotData :: (MonadIOBotData m BotData r, Has Connection r) => m ()
refreshBotData = do
  refreshCache @InfoField
  refreshCache @MinecraftAccount
  refreshCache @BowBotAccount
  refreshCache @SavedRoles
  refreshCache @DiscordAccount
  refreshCache @SnipeMessage -- TODO: this is meaningless...

updateBotData :: (MonadIOBotData m BotData r, HasAll [Manager, DiscordHandle, CounterState, Connection] r) => [StatsTimeRange] -> m ()
updateBotData times = (ask >>=) $ \ctx -> liftIO $ foldl1 concurrently_ $
  map (`runReaderT` ctx)
    [ updateDiscordAccountCache
    , do
      updateHypixelBowLeaderboards
      forM_ times updateHypixelBowTimeStats
    ]

clearBotDataCaches :: (MonadIOBotData m BotData r, Has CounterState r) => m ()
clearBotDataCaches = do
  clearCounter HypixelApi
  clearCacheSingle @HypixelGuildMembers
  clearCacheSingle @HypixelOnlinePlayers

downloadBotData :: IO BotData
downloadBotData = do
  bdt <- atomically emptyBotData
  withDB $ \conn -> runReaderT refreshBotData (conn, bdt)
  return bdt