{-# LANGUAGE TypeFamilies #-}

module BowBot.Stats where

import Data.Proxy
import Discord.Internal.Rest (UserId)
import Data.Map (Map)
import Network.HTTP.Conduit (Manager)

class StatType s where
  data Settings s
  data SettingsUpdate s
  data Leaderboards s
  requestStats :: Proxy s -> Manager -> String -> IO (Maybe s)
  showStats :: Settings s -> s -> String
  statsNotable :: s -> Bool
  toLeaderboard :: s -> Leaderboards s
  getLeaderboard :: Proxy s -> Manager -> IO (Maybe (Map String (Leaderboards s)))
  updateLeaderboard :: Manager -> Map String (Leaderboards s) -> IO ()
  defSettings :: Proxy s -> Settings s
  allSettings :: Proxy s -> Settings s
  getSettings :: Proxy s -> Manager -> IO (Maybe (Map UserId (Settings s)))
  updateSettings :: Manager -> UserId -> SettingsUpdate s -> IO ()


data BoolSense = Never | WhenSensible | Always deriving (Show, Eq, Ord, Enum)