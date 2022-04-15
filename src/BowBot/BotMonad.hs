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
import BowBot.Network.Class (MonadNetwork)
import BowBot.Discord.Class (MonadDiscord)
import BowBot.Network.Monad (NetworkT(..))
import BowBot.Discord.Monad (DiscordHandlerT(..))
import Control.Monad.Trans
import Control.Monad.Reader (MonadReader, MonadFix)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Writer.Class (MonadWriter)
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import BowBot.BotData.Basic
import BowBot.BotData.Cached (MonadCache)
import BowBot.BotData.Counter (MonadCounter, Counted)

newtype BotT m a = BotT { runBotT :: BotData -> Manager -> DiscordHandle -> m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadHoistIO, MonadNetwork, MonadDiscord, MonadError e,
            MonadState s, MonadWriter w, MonadFail, MonadFix, Alternative, MonadPlus, MonadReader r) via (BotDataT (NetworkT (DiscordHandlerT m)))

instance MonadTrans BotT where
  lift f = BotT $ \_ _ _ -> f

deriving via (BotDataT (NetworkT (DiscordHandlerT m))) instance (MonadCache c (BotDataT (NetworkT (DiscordHandlerT m))), MonadIO (BotT m)) => MonadCache c (BotT m)

deriving via (BotDataT (NetworkT (DiscordHandlerT m))) instance (Counted c, MonadCounter c (BotDataT (NetworkT (DiscordHandlerT m))), MonadIO (BotT m)) => MonadCounter c (BotT m)

type Bot = BotT IO