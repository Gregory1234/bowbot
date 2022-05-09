{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module BowBot.BotMonad where

import BowBot.Utils
import Discord
import Network.HTTP.Conduit (Manager)
import Control.Monad.Trans
import Control.Monad.Reader (ReaderT(..), MonadReader, MonadFix)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Writer.Class (MonadWriter)
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import BowBot.BotData.Basic
import BowBot.BotData.Cached (MonadCache)
import BowBot.BotData.Counter (MonadCounter, Counted)
import BowBot.BotData.CachedSingle (MonadCacheSingle)

newtype BotT m a = BotT { runBotT :: BotData -> (Manager, DiscordHandle) -> m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadHoistIO, MonadReader (Manager, DiscordHandle), MonadError e,
            MonadState s, MonadWriter w, MonadFail, MonadFix, Alternative, MonadPlus) via (BotDataT (ReaderT (Manager, DiscordHandle) m))

instance MonadTrans BotT where
  lift f = BotT $ \_ _ -> f

deriving via (BotDataT (ReaderT (Manager, DiscordHandle) m)) instance (MonadCache c (BotDataT (ReaderT (Manager, DiscordHandle) m)), MonadIO (BotT m)) => MonadCache c (BotT m)

deriving via (BotDataT (ReaderT (Manager, DiscordHandle) m)) instance (Counted c, MonadCounter c (BotDataT (ReaderT (Manager, DiscordHandle) m)), MonadIO (BotT m)) => MonadCounter c (BotT m)

deriving via (BotDataT (ReaderT (Manager, DiscordHandle) m)) instance (MonadCacheSingle c (BotDataT (ReaderT (Manager, DiscordHandle) m)), MonadIO (BotT m)) => MonadCacheSingle c (BotT m)

type Bot = BotT IO