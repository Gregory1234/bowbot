module BowBot.BotMonad where

import Discord
import Network.HTTP.Conduit (Manager)
import Control.Monad.Reader (ReaderT(..))
import Data.Has
import BowBot.Counter.Basic
import BowBot.DB.Basic (SafeMysqlConn)
import BowBot.BotData.Info
import BowBot.Ranked.Queue

data BotContext = BotContext
  { bctxManager :: !Manager
  , bctxConnection :: !SafeMysqlConn
  , bctxDiscord :: !DiscordHandle
  , bctxCounter :: !CounterState
  , bctxInfo :: !InfoCache
  , bctxGameQueue :: !GameQueue
  }

instance Has Manager BotContext where
  getter = bctxManager
  modifier f x = x { bctxManager = f $ bctxManager x }
instance Has SafeMysqlConn BotContext where
  getter = bctxConnection
  modifier f x = x { bctxConnection = f $ bctxConnection x }
instance Has DiscordHandle BotContext where
  getter = bctxDiscord
  modifier f x = x { bctxDiscord = f $ bctxDiscord x }
instance Has CounterState BotContext where
  getter = bctxCounter
  modifier f x = x { bctxCounter = f $ bctxCounter x }
instance Has InfoCache BotContext where
  getter = bctxInfo
  modifier f x = x { bctxInfo = f $ bctxInfo x }
instance Has GameQueue BotContext where
  getter = bctxGameQueue
  modifier f x = x { bctxGameQueue = f $ bctxGameQueue x }

type BotT = ReaderT BotContext

type Bot = BotT IO