{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BowBot.BotData.Basic where

import BowBot.BotData.Info
import BowBot.BotData.Cached
import BowBot.Minecraft.Account
import BowBot.Command.Basic
import BowBot.Account.Basic
import BowBot.Hypixel.Basic
import BowBot.Settings.Basic
import BowBot.Hypixel.Leaderboard
import BowBot.Discord.Roles
import BowBot.BotData.CachedSingle
import BowBot.Hypixel.Guild
import BowBot.Discord.Account
import BowBot.Hypixel.TimeStats
import Data.Default
import BowBot.Hypixel.Watchlist
import BowBot.Birthday.Basic
import BowBot.Snipe.Basic
import BowBot.BotData.HasData

data BotData = BotData
  { infoFieldCache :: DatabaseCache InfoField
  , minecraftAccountCache :: DatabaseCache MinecraftAccount
  , bowBotAccountCache :: DatabaseCache BowBotAccount
  , settingsCache :: DatabaseCache Settings
  , hypixelLeaderboardCache :: DatabaseCache HypixelBowLeaderboardEntry
  , savedRolesCache :: DatabaseCache SavedRoles
  , hypixelGuildMembersCache :: CachedData HypixelGuildMembers
  , discordAccountsCache :: DatabaseCache DiscordAccount
  , hypixelDailyStatsCache :: DatabaseCache (HypixelBowTimeStats 'DailyStats)
  , hypixelWeeklyStatsCache :: DatabaseCache (HypixelBowTimeStats 'WeeklyStats)
  , hypixelMonthlyStatsCache :: DatabaseCache (HypixelBowTimeStats 'MonthlyStats)
  , hypixelOnlinePlayersCache :: CachedData HypixelOnlinePlayers
  , birthdayCache :: DatabaseCache BirthdayDate
  , snipeCache :: DatabaseCache SnipeMessage
  }

instance Has (DatabaseCache InfoField) BotData where
  getter = infoFieldCache
  modifier f x = x { infoFieldCache = f $ infoFieldCache x }

instance Has (DatabaseCache MinecraftAccount) BotData where
  getter = minecraftAccountCache
  modifier f x = x { minecraftAccountCache = f $ minecraftAccountCache x }

instance Has (DatabaseCache BowBotAccount) BotData where
  getter = bowBotAccountCache
  modifier f x = x { bowBotAccountCache = f $ bowBotAccountCache x }

instance Has (DatabaseCache Settings) BotData where
  getter = settingsCache
  modifier f x = x { settingsCache = f $ settingsCache x }

instance Has (DatabaseCache HypixelBowLeaderboardEntry) BotData where
  getter = hypixelLeaderboardCache
  modifier f x = x { hypixelLeaderboardCache = f $ hypixelLeaderboardCache x }

instance Has (DatabaseCache SavedRoles) BotData where
  getter = savedRolesCache
  modifier f x = x { savedRolesCache = f $ savedRolesCache x }

instance Has (CachedData HypixelGuildMembers) BotData where
  getter = hypixelGuildMembersCache
  modifier f x = x { hypixelGuildMembersCache = f $ hypixelGuildMembersCache x }

instance Has (DatabaseCache DiscordAccount) BotData where
  getter = discordAccountsCache
  modifier f x = x { discordAccountsCache = f $ discordAccountsCache x }

instance (Default (SStatsTimeRange t)) => Has (DatabaseCache (HypixelBowTimeStats t)) BotData where
  getter = case def :: SStatsTimeRange t of
    SDailyStats -> hypixelDailyStatsCache
    SWeeklyStats -> hypixelWeeklyStatsCache
    SMonthlyStats -> hypixelMonthlyStatsCache
  modifier f x = case def :: SStatsTimeRange t of
    SDailyStats -> x { hypixelDailyStatsCache = f $ hypixelDailyStatsCache x }
    SWeeklyStats -> x { hypixelWeeklyStatsCache = f $ hypixelWeeklyStatsCache x }
    SMonthlyStats -> x { hypixelMonthlyStatsCache = f $ hypixelMonthlyStatsCache x }

instance Has (CachedData HypixelOnlinePlayers) BotData where
  getter = hypixelOnlinePlayersCache
  modifier f x = x { hypixelOnlinePlayersCache = f $ hypixelOnlinePlayersCache x }

instance Has (DatabaseCache BirthdayDate) BotData where
  getter = birthdayCache
  modifier f x = x { birthdayCache = f $ birthdayCache x }

instance Has (DatabaseCache SnipeMessage) BotData where
  getter = snipeCache
  modifier f x = x { snipeCache = f $ snipeCache x }

instance HasBotData BotData BotData