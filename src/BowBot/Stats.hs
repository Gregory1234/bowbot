{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.Stats(
  module BowBot.Stats, module Data.Proxy, Map, fromList, Manager, module BowBot.Settings
) where

import Data.Proxy
import Data.Map (Map, fromList)
import Network.HTTP.Conduit (Manager)
import Data.Foldable (for_)
import BowBot.Settings

class (Show s, Show (Leaderboards s)) => StatType s where
  data Leaderboards s
  requestStats :: Proxy s -> Manager -> String -> IO (Maybe s)
  showStats :: Settings -> s -> String
  statsNotable :: s -> Bool
  toLeaderboard :: s -> Leaderboards s
  getLeaderboard :: Proxy s -> Manager -> IO (Maybe (Map String (Leaderboards s)))
  updateLeaderboard :: Manager -> Map String (Leaderboards s) -> IO ()
  banLeaderboard :: Proxy s -> Manager -> String -> IO (Maybe Bool)

fullUpdateStats :: StatType s => Proxy s -> Manager -> String -> IO ()
fullUpdateStats pr man uuid = do
  stats <- requestStats pr man uuid
  for_ stats $ \x -> updateLeaderboard man $ fromList [(uuid, toLeaderboard x)]
