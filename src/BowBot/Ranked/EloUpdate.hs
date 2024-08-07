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
import BowBot.Ranked.Queue

calculateEloChanges :: (RankedBowStats, RankedBowStats) -> RankedBowScore -> (Integer, Integer)
calculateEloChanges (stats1, stats2) score = (eloChange player1win (rankedElo stats1), eloChange (not player1win) (rankedElo stats2))
  where
    clampElo True = clamp (5, 25)
    clampElo False = clamp (-25, -5)
    avgElo = (rankedElo stats1 + rankedElo stats2) `quot` 2
    winDiff = abs $ rankedScore1 score - rankedScore2 score
    eloChangeMod elo
      | winDiff == 1 = elo `quot` 2
      | winDiff == 3 = (elo * 3) `quot` 2
      | otherwise = elo
    eloChange win elo = clampElo win $ eloChangeMod $ (if win then 15 else -15) - ((elo - avgElo) `quot` 20)
    player1win = rankedScore1 score > rankedScore2 score

applyEloChange :: RankedBowStats -> Integer -> Integer -> Integer -> RankedBowStats
applyEloChange RankedBowStats {..} wins losses change = let winner = wins > losses; newWinstreak = if winner then rankedCurrentWinstreak + 1 else 0 in RankedBowStats
  { rankedQueue = rankedQueue
  , rankedElo = rankedElo + change
  , rankedWins = rankedWins + (if winner then 1 else 0)
  , rankedLosses = rankedLosses + (if winner then 0 else 1)
  , rankedSmallWins = rankedSmallWins + wins
  , rankedSmallLosses = rankedSmallLosses + losses
  , rankedBestWinstreak = max rankedBestWinstreak newWinstreak
  , rankedCurrentWinstreak = newWinstreak
  }

applyEloByScore :: (MonadIOReader m r, Has SafeMysqlConn r) => RankedBowGame -> RankedBowScore -> m (Maybe [(BowBotId, Integer, Integer)])
applyEloByScore RankedBowGame { rankedPlayers = (player1, player2), rankedGameQueue } score@(RankedBowScore score1 score2) = do
  stats <- queryLog [mysql|SELECT `account_id`, RankedBowStats FROM `ranked_bow_stats` WHERE `account_id` IN (player1, player2) AND `queue` = rankedGameQueue|]
  case (lookup player1 stats, lookup player2 stats) of
    (Just stats1, Just stats2) -> do
      let (change1, change2) = calculateEloChanges (stats1, stats2) score
      let (newStats1, newStats2) = (applyEloChange stats1 score1 score2 change1, applyEloChange stats2 score2 score1 change2)
      c <- (>0) <$> executeLog [mysql|INSERT INTO `ranked_bow_stats`(`account_id`, RankedBowStats) VALUES (player1, newStats1), (player2, newStats2)|]
      return $ if c then Just [(player1, rankedElo stats1, change1), (player2, rankedElo stats2, change2)] else Nothing
    _ -> return Nothing

applyPureEloUpdate :: (MonadIOReader m r, Has SafeMysqlConn r) => QueueName -> [(BowBotId, Integer)] -> m (Maybe [(BowBotId, Integer, Integer)])
applyPureEloUpdate _ [] = pure (Just [])
applyPureEloUpdate queue updates = do
  let players = map fst updates
  stats <- queryLog [mysql|SELECT `account_id`, RankedBowStats FROM `ranked_bow_stats` WHERE `account_id` IN players AND `queue` = queue|]
  let newStats = map (\(bid, eloUpdate) -> let s = fromMaybe (defRankedBowStats queue) (lookup bid stats) in (bid, s { rankedElo = rankedElo s + eloUpdate })) updates
  c <- (>0) <$> executeLog [mysql|INSERT INTO `ranked_bow_stats`(`account_id`, RankedBowStats) VALUES newStats..|]
  if c then pure $ Just [(bid, rankedElo s, update) | (bid, update) <- updates, let s = fromMaybe (defRankedBowStats queue) (lookup bid stats)] else pure Nothing

eloChangesChannelInfo :: InfoType ChannelId
eloChangesChannelInfo = InfoType { infoName = "ranked_bow_elo_changes_channel", infoDefault = 0, infoParse = first pack . readEither . unpack }

createEloUpdateMessage :: QueueName -> Maybe (Bool, Integer) -> [(Text, Integer, Integer)] -> Text
createEloUpdateMessage q headerInfo updates = let
    header = case headerInfo of
      Nothing -> "**Elo changed:\n**"
      Just (success, gameId) -> "**Game #" <> showt gameId <> (if success then " finished (" else " abandoned (") <> T.toUpper (queueName q) <> "):**\n"
  in header <> (if null updates then "*No elo changes*" else T.unlines (map (\(n,c,v) -> n <> ": " <> showt c <> " " <> (if v < 0 then showt v else "+" <> showt v)) updates))

announceEloUpdate :: (MonadIOReader m r, HasAll '[SafeMysqlConn, InfoCache, DiscordHandle] r) => QueueName -> Maybe (Bool, Integer) -> [(BowBotId, Integer, Integer)] -> m Bool
announceEloUpdate q headerInfo updates = do
  eloChangesChannel <- askInfo eloChangesChannelInfo
  let accIds = map (\(x,_,_) -> x) updates
  names <- queryLog [mysql|SELECT `account_id`, MinecraftAccount FROM `minecraft` JOIN `ranked_bow` ON `ranked_uuid` = `uuid` WHERE `account_id` IN accIds|]
  let msgText = createEloUpdateMessage q headerInfo (mapMaybe (\(i, c, u) -> (, c, u) . head . mcNames <$> lookup i names) updates)
  msg' <- call $ R.CreateMessage eloChangesChannel msgText
  case msg' of
    Left err -> do
      logError $ showt err
      return False
    Right msg -> if null updates then return True else do
      let fullUpdates = map (\(i, c, u) -> (i, messageId msg, c, u)) updates
      (>0) <$> executeLog [mysql|INSERT INTO `ranked_bow_elo`(`account_id`, `message_id`, `prior_elo`, `elo_change`) VALUES fullUpdates..|]