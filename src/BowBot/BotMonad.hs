{-# LANGUAGE DerivingVia #-}

module BowBot.BotMonad where

import BowBot.Utils
import Discord
import Network.HTTP.Conduit (Manager)
import Database.MySQL.Simple (Connection)
import BowBot.Network.Class (MonadNetwork)
import BowBot.DB.Class (MonadDB)
import BowBot.Discord.Class (MonadDiscord)
import BowBot.Network.Monad (NetworkT(..))
import BowBot.DB.Monad (DatabaseT(..))
import BowBot.Discord.Monad (DiscordHandlerT(..))
import Control.Monad.Trans
import Control.Monad.Reader (MonadReader, MonadFix)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Writer.Class (MonadWriter)
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)

newtype BotT m a = BotT { runBotT :: Connection -> Manager -> DiscordHandle -> m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadHoistIO, MonadNetwork, MonadDB, MonadDiscord, MonadError e,
            MonadState s, MonadWriter w, MonadFail, MonadFix, Alternative, MonadPlus, MonadReader r) via (DatabaseT (NetworkT (DiscordHandlerT m)))

instance MonadTrans BotT where
  lift f = BotT $ \_ _ _ -> f

type Bot = BotT IO