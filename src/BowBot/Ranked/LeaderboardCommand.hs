{-# LANGUAGE QuasiQuotes #-}

module BowBot.Ranked.LeaderboardCommand where

import BowBot.Utils
import BowBot.DB.Basic
import qualified Data.HashMap.Strict as HM
import BowBot.Minecraft.Basic
import BowBot.Ranked.Stats
import BowBot.Hypixel.LeaderboardCommand

getRankedBowLeaderboards :: (MonadIOReader m r, HasAll '[SafeMysqlConn] r) => m (HM.HashMap UUID RankedBowStats)
getRankedBowLeaderboards = HM.fromList <$> queryLog [mysql|SELECT `ranked_uuid`, RankedBowStats FROM `ranked_bow_stats`|]

rankedEloLeaderboardType :: LeaderboardType RankedBowStats Integer
rankedEloLeaderboardType = LeaderboardType "Ranked Bow Duels Elo" False "Elo" showt getRankedBowLeaderboards (fmap rankedElo . filterMaybe requirements)
  where
    requirements RankedBowStats {..} = rankedWins /= 0 || rankedLosses /= 0

rankedWinsLeaderboardType :: LeaderboardType RankedBowStats Integer
rankedWinsLeaderboardType = LeaderboardType "Ranked Bow Duels Wins" False "Wins" showt getRankedBowLeaderboards (filterMaybe (/= 0) . rankedWins)

rankedLossesLeaderboardType :: LeaderboardType RankedBowStats Integer
rankedLossesLeaderboardType = LeaderboardType "Ranked Bow Duels Losses" False "Losses" showt getRankedBowLeaderboards (filterMaybe (/= 0) . rankedLosses)

rankedWlrLeaderboardType :: LeaderboardType RankedBowStats (WLR Integer)
rankedWlrLeaderboardType = LeaderboardType "Ranked Bow Duels WLR" False "WLR" showWLR getRankedBowLeaderboards (fmap rankedWLR . filterMaybe requirements)
  where
    requirements RankedBowStats {..} = rankedWins /= 0 || rankedLosses /= 0
