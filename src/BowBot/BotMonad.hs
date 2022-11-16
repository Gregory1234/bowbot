{-# LANGUAGE MultiParamTypeClasses #-}

module BowBot.BotMonad where

import Discord
import Network.HTTP.Conduit (Manager)
import Control.Monad.Reader (ReaderT(..))
import BowBot.BotData.Basic
import Data.Has
import BowBot.BotData.HasData
import BowBot.DB.Basic (Connection)

data BotContext = BotContext
  { bctxManager :: Manager
  , bctxConnection :: Connection
  , bctxDiscord :: DiscordHandle
  , bctxData :: BotData
  }

instance Has Manager BotContext where
  getter = bctxManager
  modifier f x = x { bctxManager = f $ bctxManager x }
instance Has Connection BotContext where
  getter = bctxConnection
  modifier f x = x { bctxConnection = f $ bctxConnection x }
instance Has DiscordHandle BotContext where
  getter = bctxDiscord
  modifier f x = x { bctxDiscord = f $ bctxDiscord x }
instance Has BotData BotContext where
  getter = bctxData
  modifier f x = x { bctxData = f $ bctxData x }
instance HasBotData BotData BotContext

type BotT = ReaderT BotContext

type Bot = BotT IO