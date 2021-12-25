{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.Stats(
  module BowBot.Stats, module Data.Proxy, Map, fromList, Manager, module BowBot.Settings
) where

import Data.Proxy
import Data.Map (Map, fromList)
import BowBot.Settings
import BowBot.API

class (Show s, Show (Leaderboards s)) => StatType s where
  data Leaderboards s
  requestStats :: APIMonad m => Proxy s -> String -> m (Maybe s)
  showStats :: Settings -> s -> String
  statsNotable :: s -> Bool
  toLeaderboard :: s -> Leaderboards s
  getLeaderboard :: APIMonad m => Proxy s -> m (Maybe (Map String (Leaderboards s)))
  updateLeaderboard :: APIMonad m => Map String (Leaderboards s) -> m ()
  banLeaderboard :: APIMonad m => Proxy s -> String -> m (Maybe Bool)

fullUpdateStats :: (StatType s, APIMonad m) => Proxy s -> String -> m ()
fullUpdateStats pr uuid = do
  stats <- requestStats pr uuid
  for_ stats $ \x -> updateLeaderboard $ fromList [(uuid, toLeaderboard x)]
