{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module BowBot.Ranked.Stats where

import BowBot.Utils
import BowBot.DB.Basic
import BowBot.Settings.Basic
import BowBot.Account.Basic
import qualified Data.Text as T
import BowBot.Minecraft.Basic (UUID)
import BowBot.Ranked.Queue
import BowBot.Minecraft.Account

data RankedBowStats = RankedBowStats
  { rankedQueue :: QueueName
  , rankedWins :: Integer
  , rankedLosses :: Integer
  , rankedSmallWins :: Integer
  , rankedSmallLosses :: Integer
  , rankedBestWinstreak :: Integer
  , rankedCurrentWinstreak :: Integer
  , rankedElo :: Integer
  } deriving stock (Show, Eq, Generic)
    deriving (ToMysql, FromMysql) via (Generically RankedBowStats)

defRankedBowStats :: QueueName -> RankedBowStats
defRankedBowStats q = RankedBowStats q 0 0 0 0 0 0 1400

$(pure [])

rankedWLR :: RankedBowStats -> WLR Integer
rankedWLR RankedBowStats {..} = WLR rankedWins rankedLosses

getRankedBowStatsByBowBot :: (MonadIOReader m r, Has SafeMysqlConn r) => QueueName -> BowBotId -> m (Maybe RankedBowStats)
getRankedBowStatsByBowBot queue bid = queryOnlyLog [mysql|SELECT RankedBowStats FROM `ranked_bow_stats` WHERE `account_id` = bid AND `queue` = queue|]

getRankedMinecraftAccountByBowBotId :: (MonadIOReader m r, Has SafeMysqlConn r) => BowBotId -> m (Maybe MinecraftAccount)
getRankedMinecraftAccountByBowBotId bid = queryOnlyLog [mysql|SELECT MinecraftAccount FROM `minecraft` JOIN `ranked_bow` ON `ranked_uuid` = `uuid` WHERE `account_id` = bid|]

addRankedPlayer :: (MonadIOReader m r, Has SafeMysqlConn r) => BowBotId -> UUID -> m Bool
addRankedPlayer bid uuid = (>0) <$> executeLog [mysql|INSERT INTO `ranked_bow`(`account_id`, `ranked_uuid`) VALUES (bid,uuid)|]

showRankedBowStats :: Settings -> RankedBowStats -> Text
showRankedBowStats Settings {..} RankedBowStats {..} = T.unlines $ catMaybes
  [ Just $ "- Queue " <> T.toUpper (queueName rankedQueue) <> ":"
  , Just -- TODO: add to settings
  $ " - *Ranked Bow Elo:* **"
  <> showt rankedElo
  <> "**"
  , onlyIfBin sWins
  $ " - *Ranked Bow Wins:* **"
  <> showt rankedWins
  <> "**"
  , onlyIfBin sLosses
  $ " - *Ranked Bow Losses:* **"
  <> showt rankedLosses
  <> "**"
  , onlyIfBin sWins
  $ " - *Ranked Bow Small Wins:* **"
  <> showt rankedSmallWins
  <> "**"
  , onlyIfBin sLosses
  $ " - *Ranked Bow Small Losses:* **"
  <> showt rankedSmallLosses
  <> "**"
  , onlyIfTer sWLR (rankedWins + rankedLosses /= 0)
  $ " - *Ranked Bow Win/Loss Ratio:* **"
  <> winLossRatio
  <> "**"
  , onlyIfTer sBestStreak True
  $ " - *Best Ranked Bow Winstreak:* **"
  <> showt rankedBestWinstreak
  <> "**"
  , onlyIfTer sCurrentStreak True
  $ " - *Current Ranked Bow Winstreak:* **"
  <> showt rankedCurrentWinstreak
  <> "**"
  ]
  where
    winLossRatio = showWLR (WLR rankedWins rankedLosses)