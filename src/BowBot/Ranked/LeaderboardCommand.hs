{-# LANGUAGE QuasiQuotes #-}

module BowBot.Ranked.LeaderboardCommand where

import BowBot.Utils
import BowBot.DB.Basic
import qualified Data.HashMap.Strict as HM
import BowBot.Minecraft.Basic
import BowBot.Ranked.Stats
import BowBot.Hypixel.LeaderboardCommand
import BowBot.Account.Basic
import BowBot.Ranked.Queue
import BowBot.Command
import Data.Bifunctor (second)
import qualified Data.Map as M

data RankedLeaderboardType q s a = RankedLeaderboardType
  { rankedLeaderboardName :: !Text
  , rankedLeaderboardStatName :: !Text
  , rankedLeaderboardShowValue :: a -> Text
  , rankedLeaderboardGetStats :: CommandHandler (HM.HashMap UUID [(q, s)])
  , rankedLeaderboardParser :: s -> Maybe a
  , rankedLeaderboardParseQueue :: Text -> CommandHandler (Maybe q)
  , rankedLeaderboardShowQueue :: q -> Text
  , rankedLeaderboardCombine :: Maybe ([(q,Maybe a)] -> Maybe a)
  }

rankedLeaderboardDescription :: RankedLeaderboardType q s a -> Text
rankedLeaderboardDescription RankedLeaderboardType {..} = "show " <> rankedLeaderboardName <> " leaderboard in a given queue"

rankedLeaderboardDescriptionCombined :: RankedLeaderboardType q s a -> Text
rankedLeaderboardDescriptionCombined RankedLeaderboardType {..} = "show combined " <> rankedLeaderboardName <> " leaderboard"

rankedLeaderboardTypeToLeaderboardType :: Eq q => q -> RankedLeaderboardType q s a -> LeaderboardType s a
rankedLeaderboardTypeToLeaderboardType q RankedLeaderboardType {..} = LeaderboardType
  { leaderboardName = rankedLeaderboardName <> " (" <> rankedLeaderboardShowQueue q <> ")"
  , leaderboardIsGuild = False
  , leaderboardStatName = rankedLeaderboardStatName
  , leaderboardShowValue = rankedLeaderboardShowValue
  , leaderboardGetStats = HM.mapMaybe (lookup q) <$> rankedLeaderboardGetStats
  , leaderboardParser = rankedLeaderboardParser
  }

rankedLeaderboardTypeToLeaderboardTypeCombined :: ([(q,Maybe a)] -> Maybe a) -> RankedLeaderboardType q s a -> LeaderboardType [(q,s)] a
rankedLeaderboardTypeToLeaderboardTypeCombined combine RankedLeaderboardType {..} = LeaderboardType
  { leaderboardName = rankedLeaderboardName <> " (combined)"
  , leaderboardIsGuild = False
  , leaderboardStatName = rankedLeaderboardStatName
  , leaderboardShowValue = rankedLeaderboardShowValue
  , leaderboardGetStats = rankedLeaderboardGetStats
  , leaderboardParser = combine . map (second rankedLeaderboardParser)
  }

rankedLeaderboardCommand :: forall q s a. (Eq q, Ord a) => RankedLeaderboardType q s a -> Text -> Command
rankedLeaderboardCommand lbt@RankedLeaderboardType {..} name = Command CommandInfo
  { commandName = name
  , commandHelpEntries =
    (HelpEntry { helpUsage = name <> " [queue] [name|page|\"all\"]", helpDescription = rankedLeaderboardDescription lbt, helpGroup = "ranked" })
    : [ HelpEntry { helpUsage = name <> " [name|page|\"all\"]", helpDescription = rankedLeaderboardDescription lbt, helpGroup = "ranked" } | isJust rankedLeaderboardCombine ]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ twoOptionalArguments $ \case
    Nothing -> do
      combine <- liftMaybe "*You have to provide a queue!*" rankedLeaderboardCombine
      let lbt' = rankedLeaderboardTypeToLeaderboardTypeCombined combine lbt
      lift $ commandHandler $ leaderboardCommand lbt' name
    Just (q, _) -> do
      qn' <- lift $ rankedLeaderboardParseQueue q
      case qn' of
        Nothing -> do
          combine <- liftMaybe "*You have to provide a queue!*" rankedLeaderboardCombine
          let lbt' = rankedLeaderboardTypeToLeaderboardTypeCombined combine lbt
          lift $ commandHandler $ leaderboardCommand lbt' name
        Just qn -> do
          let lbt' = rankedLeaderboardTypeToLeaderboardType qn lbt
          lift $ ignoreFirstArgument $ commandHandler $ leaderboardCommand lbt' name

getRankedBowLeaderboards :: (MonadIOReader m r, HasAll '[SafeMysqlConn] r) => m (HM.HashMap UUID [(QueueName, RankedBowStats)])
getRankedBowLeaderboards = HM.fromList . M.toList . M.map (map snd) . groupByToMap fst
  <$> queryLog [mysql|SELECT `ranked_uuid`, (`queue`, RankedBowStats) FROM `ranked_bow_stats` JOIN `ranked_bow` ON `account_id` = `ranked_bow_stats`.`account_id`|]

rankedEloLeaderboardType :: RankedLeaderboardType QueueName RankedBowStats Integer
rankedEloLeaderboardType = RankedLeaderboardType "Ranked Bow Duels Elo" "Elo" showt getRankedBowLeaderboards (fmap rankedElo . filterMaybe requirements) getQueueByName queueName Nothing 
  where
    requirements RankedBowStats {..} = rankedWins /= 0 || rankedLosses /= 0

combineBySum :: Num a => [(q,Maybe a)] -> Maybe a
combineBySum (map snd -> xs) = if all isNothing xs then Nothing else Just $ sum $ catMaybes xs

rankedWinsLeaderboardType :: RankedLeaderboardType QueueName RankedBowStats Integer
rankedWinsLeaderboardType = RankedLeaderboardType "Ranked Bow Duels Wins" "Wins" showt getRankedBowLeaderboards (filterMaybe (/= 0) . rankedWins) getQueueByName queueName (Just combineBySum)

rankedLossesLeaderboardType :: RankedLeaderboardType QueueName RankedBowStats Integer
rankedLossesLeaderboardType = RankedLeaderboardType "Ranked Bow Duels Losses" "Losses" showt getRankedBowLeaderboards (filterMaybe (/= 0) . rankedLosses) getQueueByName queueName (Just combineBySum)

rankedWlrLeaderboardType :: RankedLeaderboardType QueueName RankedBowStats (WLR Integer)
rankedWlrLeaderboardType = RankedLeaderboardType "Ranked Bow Duels WLR" "WLR" showWLR getRankedBowLeaderboards (fmap rankedWLR . filterMaybe requirements) getQueueByName queueName (Just combine)
  where
    requirements RankedBowStats {..} = rankedWins /= 0 || rankedLosses /= 0
    combine (map snd -> xs) = if all isNothing xs then Nothing else Just $ WLR (sum $ map wlrWins $ catMaybes xs) (sum $ map wlrLosses $ catMaybes xs)
