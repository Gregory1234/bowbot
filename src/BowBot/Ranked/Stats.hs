{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module BowBot.Ranked.Stats where

import BowBot.Utils
import BowBot.DB.Basic
import BowBot.Settings.Basic
import BowBot.Account.Basic
import qualified Data.Text as T
import BowBot.Minecraft.Basic (UUID)

data RankedBowStats = RankedBowStats
  { rankedWins :: Integer
  , rankedLosses :: Integer
  , rankedElo :: Integer
  } deriving stock (Show, Eq, Generic)
    deriving (ToMysql, FromMysql) via (Generically RankedBowStats)

$(pure [])

rankedWLR :: RankedBowStats -> WLR Integer
rankedWLR RankedBowStats {..} = WLR rankedWins rankedLosses

getRankedBowStatsByBowBot :: (MonadIOReader m r, Has SafeMysqlConn r) => BowBotId -> m (Maybe RankedBowStats)
getRankedBowStatsByBowBot bid = queryOnlyLog [mysql|SELECT RankedBowStats FROM `ranked_bow_stats` WHERE `account_id` = bid|]

addRankedPlayer :: (MonadIOReader m r, Has SafeMysqlConn r) => BowBotId -> UUID -> m Bool
addRankedPlayer bid uuid = (>0) <$> executeLog [mysql|INSERT INTO `ranked_bow_stats`(`account_id`, `ranked_uuid`) VALUES (bid,uuid)|]

showRankedBowStats :: Settings -> RankedBowStats -> Text
showRankedBowStats Settings {..} RankedBowStats {..} = T.unlines $ catMaybes
  [ onlyIfBin sWins
  $ "- *Ranked Bow Wins:* **"
  <> showt rankedWins
  <> "**"
  , onlyIfBin sLosses
  $ "- *Ranked Bow Losses:* **"
  <> showt rankedLosses
  <> "**"
  , onlyIfTer sWLR (rankedWins + rankedLosses /= 0)
  $ "- *Ranked Bow Win/Loss Ratio:* **"
  <> winLossRatio
  <> "**"
  , Just -- TODO: add to settings
  $ "- *Ranked Bow Elo:* **"
  <> showt rankedElo
  <> "**"
  ]
  where
    winLossRatio = showWLR (WLR rankedWins rankedLosses)