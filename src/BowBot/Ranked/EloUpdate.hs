{-# LANGUAGE QuasiQuotes #-}

module BowBot.Ranked.EloUpdate where

import BowBot.Ranked.Game
import BowBot.Ranked.Report
import BowBot.Discord.Utils
import BowBot.DB.Basic
import BowBot.Ranked.Stats
import BowBot.Account.Basic
import Data.Ord (clamp)
import BowBot.BotData.Info
import BowBot.Minecraft.Basic
import BowBot.Minecraft.Account
import qualified Data.Text as T
import qualified Discord.Requests as R
import Data.Bifunctor (first)

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

applyEloByScore :: (MonadIOReader m r, Has SafeMysqlConn r) => RankedBowGame -> RankedBowScore -> m (Maybe [(BowBotId, Integer)])
applyEloByScore RankedBowGame { rankedPlayers = (player1, player2) } score = do
  stats <- queryLog [mysql|SELECT `account_id`, RankedBowStats FROM `ranked_bow_stats` WHERE `account_id` IN (player1, player2)|]
  case (lookup player1 stats, lookup player2 stats) of
    (Just stats1, Just stats2) -> do
      let (change1, change2) = calculateEloChanges (stats1, stats2) score
      let (newStats1, newStats2) = (applyEloChange stats1 change1, applyEloChange stats2 change2)
      c <- (>0) <$> executeLog [mysql|INSERT INTO `ranked_bow_stats`(`account_id`, RankedBowStats) VALUES (player1,newStats1), (player2, newStats2)|]
      return $ if c then Just [(player1, change1), (player2, change2)] else Nothing
    _ -> return Nothing

eloChangesChannelInfo :: InfoType ChannelId
eloChangesChannelInfo = InfoType { infoName = "ranked_bow_elo_changes_channel", infoDefault = 0, infoParse = first pack . readEither . unpack }

createEloUpdateMessage :: Integer -> [(Text, Integer)] -> Text
createEloUpdateMessage gameId updates = 
  "**Game #" <> showt gameId <> " finished:**\n" 
  <> T.unlines (map (\(n,v) -> n <> ": " <> (if v < 0 then showt v else "+" <> showt v)) updates)

announceEloUpdate :: (MonadIOReader m r, HasAll '[SafeMysqlConn, InfoCache, DiscordHandle] r) => Integer -> [(BowBotId, Integer)] -> m Bool
announceEloUpdate gameId updates = do
  eloChangesChannel <- askInfo eloChangesChannelInfo
  let accIds = map fst updates
  names <- queryLog [mysql|SELECT `account_id`, MinecraftAccount FROM `minecraft` JOIN `ranked_bow_stats` ON `ranked_uuid` = `uuid` WHERE `account_id` IN accIds|]
  let msgText = createEloUpdateMessage gameId (mapMaybe (\(i, u) -> (, u) . head . mcNames <$> lookup i names) updates)
  msg' <- call $ R.CreateMessage eloChangesChannel msgText
  case msg' of
    Left err -> do
      logError $ showt err
      return False
    Right msg -> do
      let fullUpdates = map (\(i, u) -> (i, messageId msg, u)) updates
      (>0) <$> executeLog [mysql|INSERT INTO `ranked_bow_elo`(`account_id`, `message_id`, `elo_change`) VALUES fullUpdates..|]