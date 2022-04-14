{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

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
  
data BotData = BotData
  { infoFieldCache :: DatabaseCache InfoField
  , minecraftAccountCache :: DatabaseCache MinecraftAccount
  , permissionCache :: DatabaseCache PermissionLevel
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

instance MonadReader r m => MonadReader r (BotDataT m) where
  ask = BotDataT $ const ask
  local f (BotDataT g) = BotDataT $ local f . g