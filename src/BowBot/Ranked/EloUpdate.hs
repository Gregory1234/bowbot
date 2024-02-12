{-# LANGUAGE QuasiQuotes #-}

module BowBot.Ranked.EloUpdate where

import BowBot.Ranked.Game
import BowBot.Ranked.Report
import BowBot.Discord.Utils
import BowBot.DB.Basic
import BowBot.Ranked.Stats
import BowBot.Account.Basic
import Data.Ord (clamp)

calculateEloChanges :: (RankedBowStats, RankedBowStats) -> RankedBowScore -> (Integer, Integer)
calculateEloChanges (stats1, stats2) score = (eloChange player1win (rankedElo stats1), eloChange (not player1win) (rankedElo stats2))
  where
    clampElo True = clamp (5, 25)
    clampElo False = clamp (-25, -5)
    avgElo = (rankedElo stats1 + rankedElo stats2) `div` 2
    eloChange win elo = clampElo win $ (if win then 15 else -15) - ((elo - avgElo) `quot` 20)
    player1win = rankedScore1 score > rankedScore2 score

applyEloChange :: RankedBowStats -> Integer -> RankedBowStats
applyEloChange RankedBowStats {..} change = RankedBowStats
  { rankedElo = rankedElo + change
  , rankedWins = rankedWins + (if change > 0 then 1 else 0)
  , rankedLosses = rankedLosses + (if change < 0 then 1 else 0)
  }

applyEloByScore :: (MonadIOReader m r, Has SafeMysqlConn r) => RankedBowGame -> RankedBowScore -> m Bool
applyEloByScore RankedBowGame { rankedPlayers = (player1, player2) } score = do
  stats <- queryLog [mysql|SELECT `account_id`, RankedBowStats FROM `ranked_bow_stats` WHERE `account_id` IN (player1, player2)|]
  case (lookup player1 stats, lookup player2 stats) of
    (Just stats1, Just stats2) -> do
      let (change1, change2) = calculateEloChanges (stats1, stats2) score
      let (newStats1, newStats2) = (applyEloChange stats1 change1, applyEloChange stats2 change2)
      (>0) <$> executeLog [mysql|INSERT INTO `ranked_bow_stats`(`account_id`, RankedBowStats) VALUES (player1,newStats1), (player2, newStats2)|]
    _ -> return False