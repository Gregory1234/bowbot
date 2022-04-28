{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

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
  
data BotData = BotData
  { infoFieldCache :: DatabaseCache InfoField
  , minecraftAccountCache :: DatabaseCache MinecraftAccount
  , permissionCache :: DatabaseCache PermissionLevel
  , bowBotAccountCache :: DatabaseCache BowBotAccount
  , hypixelApiCounter :: Counter
  , settingsCache :: DatabaseCache Settings
  , hypixelLeaderboardCache :: DatabaseCache HypixelBowLeaderboardEntry
  , savedRolesCache :: DatabaseCache SavedRoles
  , hypixelGuildMembersCache :: CachedData HypixelGuildMembers
  }

newtype BotDataT m a = BotDataT { runBotDataT :: BotData -> m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadHoistIO, MonadNetwork, MonadDiscord, MonadError e, 
            MonadState s, MonadWriter w, MonadFail, MonadFix, Alternative, MonadPlus) via (ReaderT BotData m)
  deriving (MonadTrans) via (ReaderT BotData)

instance MonadIO m => MonadCache InfoField (BotDataT m) where
  getCache _ = BotDataT $ return . infoFieldCache

instance MonadIO m => MonadCache MinecraftAccount (BotDataT m) where
  getCache _ = BotDataT $ return . minecraftAccountCache

instance MonadIO m => MonadCache PermissionLevel (BotDataT m) where
  getCache _ = BotDataT $ return . permissionCache

instance MonadIO m => MonadCache BowBotAccount (BotDataT m) where
  getCache _ = BotDataT $ return . bowBotAccountCache

instance MonadIO m => MonadSimpleCounter HypixelApi (BotDataT m) where
  getCounter _ = BotDataT $ return . hypixelApiCounter

deriving via (SimpleCounter (BotDataT m)) instance MonadIO m => MonadCounter HypixelApi (BotDataT m)

instance MonadIO m => MonadCache Settings (BotDataT m) where
  getCache _ = BotDataT $ return . settingsCache

instance MonadIO m => MonadCache HypixelBowLeaderboardEntry (BotDataT m) where
  getCache _ = BotDataT $ return . hypixelLeaderboardCache

instance MonadIO m => MonadCache SavedRoles (BotDataT m) where
  getCache _ = BotDataT $ return . savedRolesCache

instance MonadIO m => MonadSimpleCacheSingle HypixelGuildMembers (BotDataT m) where
  getCachedData _ = BotDataT $ return . hypixelGuildMembersCache

deriving via (SimpleCacheSingle (BotDataT m)) instance MonadHoistIO m => MonadCacheSingle HypixelGuildMembers (BotDataT m)

instance MonadReader r m => MonadReader r (BotDataT m) where
  ask = BotDataT $ const ask
  local f (BotDataT g) = BotDataT $ local f . g