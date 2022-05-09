{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BowBot.BotData.Basic where

import BowBot.BotData.Info
import BowBot.BotData.Cached
import BowBot.Minecraft.Account
import BowBot.Utils
import Control.Monad.Cont (MonadTrans)
import Control.Monad.Reader (ReaderT(..), MonadReader(..), MonadFix)
import BowBot.Network.Class (MonadNetwork)
import BowBot.Discord.Class (MonadDiscord)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Writer.Class (MonadWriter)
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import BowBot.Command.Basic
import BowBot.Account.Basic
import BowBot.BotData.Counter
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
import Data.Has

data BotData = BotData
  { infoFieldCache :: DatabaseCache InfoField
  , minecraftAccountCache :: DatabaseCache MinecraftAccount
  , permissionCache :: DatabaseCache PermissionLevel
  , bowBotAccountCache :: DatabaseCache BowBotAccount
  , hypixelApiCounter :: Counter HypixelApi
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

instance Has (DatabaseCache PermissionLevel) BotData where
  getter = permissionCache
  modifier f x = x { permissionCache = f $ permissionCache x }

instance Has (DatabaseCache BowBotAccount) BotData where
  getter = bowBotAccountCache
  modifier f x = x { bowBotAccountCache = f $ bowBotAccountCache x }

instance Has (Counter HypixelApi) BotData where
  getter = hypixelApiCounter
  modifier f x = x { hypixelApiCounter = f $ hypixelApiCounter x }

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

instance Has (DatabaseCache BirthdayDate) BotData where
  getter = birthdayCache
  modifier f x = x { birthdayCache = f $ birthdayCache x }

instance Has (DatabaseCache SnipeMessage) BotData where
  getter = snipeCache
  modifier f x = x { snipeCache = f $ snipeCache x }

newtype BotDataT m a = BotDataT { runBotDataT :: BotData -> m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadHoistIO, MonadNetwork, MonadDiscord, MonadError e, 
            MonadState s, MonadWriter w, MonadFail, MonadFix, Alternative, MonadPlus) via (ReaderT BotData m)
  deriving (MonadTrans) via (ReaderT BotData)

instance MonadIO m => MonadCache InfoField (BotDataT m) where
  getCache' = BotDataT $ return . infoFieldCache

instance MonadIO m => MonadCache MinecraftAccount (BotDataT m) where
  getCache' = BotDataT $ return . minecraftAccountCache

instance MonadIO m => MonadCache PermissionLevel (BotDataT m) where
  getCache' = BotDataT $ return . permissionCache

instance MonadIO m => MonadCache BowBotAccount (BotDataT m) where
  getCache' = BotDataT $ return . bowBotAccountCache

instance MonadIO m => MonadCounter HypixelApi (BotDataT m) where
  getCounter _ = BotDataT $ return . hypixelApiCounter

instance MonadIO m => MonadCache Settings (BotDataT m) where
  getCache' = BotDataT $ return . settingsCache

instance MonadIO m => MonadCache HypixelBowLeaderboardEntry (BotDataT m) where
  getCache' = BotDataT $ return . hypixelLeaderboardCache

instance MonadIO m => MonadCache SavedRoles (BotDataT m) where
  getCache' = BotDataT $ return . savedRolesCache

instance MonadIO m => MonadCacheSingle HypixelGuildMembers (BotDataT m) where
  getCachedData = BotDataT $ return . hypixelGuildMembersCache

instance MonadIO m => MonadCache DiscordAccount (BotDataT m) where
  getCache' = BotDataT $ return . discordAccountsCache

instance (MonadIO m, Default (SStatsTimeRange t)) => MonadCache (HypixelBowTimeStats t) (BotDataT m) where
  getCache' = BotDataT $ return . case def :: SStatsTimeRange t of
    SDailyStats -> hypixelDailyStatsCache
    SWeeklyStats -> hypixelWeeklyStatsCache
    SMonthlyStats -> hypixelMonthlyStatsCache

instance MonadIO m => MonadCacheSingle HypixelOnlinePlayers (BotDataT m) where
  getCachedData = BotDataT $ return . hypixelOnlinePlayersCache

instance MonadIO m => MonadCache BirthdayDate (BotDataT m) where
  getCache' = BotDataT $ return . birthdayCache

instance MonadIO m => MonadCache SnipeMessage (BotDataT m) where
  getCache' = BotDataT $ return . snipeCache

instance MonadReader r m => MonadReader r (BotDataT m) where
  ask = BotDataT $ const ask
  local f (BotDataT g) = BotDataT $ local f . g