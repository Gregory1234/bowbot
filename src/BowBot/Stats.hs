{-# LANGUAGE TypeFamilies #-}

module BowBot.Stats where

import Data.Proxy
import Data.Map (Map, fromList)
import Network.HTTP.Conduit (Manager)
import Data.Foldable (for_)
import BowBot.Settings

class StatType s where
  data Leaderboards s
  requestStats :: Proxy s -> Manager -> String -> IO (Maybe s)
  showStats :: Settings -> s -> String
  statsNotable :: s -> Bool
  toLeaderboard :: s -> Leaderboards s
  getLeaderboard :: Proxy s -> Manager -> IO (Maybe (Map String (Leaderboards s)))
  updateLeaderboard :: Manager -> Map String (Leaderboards s) -> IO ()

fullUpdateStats :: StatType s => Proxy s -> Manager -> String -> IO ()
fullUpdateStats pr man uuid = do
  stats <- requestStats pr man uuid
  for_ stats $ \x -> updateLeaderboard man $ fromList [(uuid, toLeaderboard x)]
