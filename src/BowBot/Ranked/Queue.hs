{-# LANGUAGE QuasiQuotes #-}

module BowBot.Ranked.Queue where

import BowBot.Discord.Utils
import BowBot.DB.Basic
import BowBot.Account.Basic
import BowBot.BotData.Info
import Data.Coerce (coerce)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Bifunctor (first)

newtype QueueName = QueueName { queueName :: Text }
  deriving newtype (Show, Eq, Ord, ToMysqlSimple, FromMysqlSimple, ToMysql, FromMysql)

rankedModRoleInfo :: InfoType RoleId
rankedModRoleInfo = InfoType { infoName = "ranked_mod_role", infoDefault = 0, infoParse = first pack . readEither . unpack }

rankedBowQueuesInfo :: InfoType [QueueName]
rankedBowQueuesInfo = InfoType { infoName = "ranked_bow_queues", infoDefault = [], infoParse = Right . coerce . T.splitOn "," }

getQueueByName :: (MonadIOReader m r, Has InfoCache r) => Text -> m (Maybe QueueName)
getQueueByName name = do
  rankedBowQueues <- askInfo rankedBowQueuesInfo
  return $ if QueueName name `elem` rankedBowQueues then Just $ QueueName name else Nothing

getCurrentQueuesByBowBotId :: (MonadIOReader m r, Has SafeMysqlConn r) => BowBotId -> m [QueueName]
getCurrentQueuesByBowBotId bid = queryLog [mysql|SELECT `queue` FROM `ranked_bow_stats` WHERE `account_id` = bid AND `in_queue`|]

data AddToQueueRes = AddedToQueue Int | AlreadyInQueue | CurrentlyInGame Integer | QueueFilled [BowBotId]

addToQueue :: (MonadIOReader m r, Has SafeMysqlConn r) => Int -> QueueName -> BowBotId -> m (Maybe AddToQueueRes)
addToQueue limit queue bid = do
  r <- queryOnlyLog [mysql|SELECT `in_queue`,`current_game` FROM `ranked_bow` JOIN `ranked_bow_stats` ON `account_id` = `ranked_bow`.`account_id` WHERE `ranked_bow_stats`.`account_id` = bid AND `queue` = queue|]
  case r of
    Nothing -> return Nothing
    Just (False, Just g) -> return $ Just $ CurrentlyInGame g
    Just (True, _) -> return $ Just AlreadyInQueue
    Just (False, Nothing) -> do
      queueMembers <- queryLog [mysql|SELECT `account_id` FROM `ranked_bow_stats` WHERE `queue` = queue AND `in_queue`|]
      if length queueMembers == limit - 1
        then do
          let queueMembersAll = bid : queueMembers
          void $ executeLog [mysql|UPDATE `ranked_bow_stats` SET `in_queue` = 0 WHERE `account_id` IN queueMembersAll|]
          return $ Just $ QueueFilled queueMembersAll
        else do
          void $ executeLog [mysql|INSERT INTO `ranked_bow_stats`(`account_id`, `queue`, ^`in_queue`) VALUES (bid, queue, 1)|]
          return $ Just $ AddedToQueue (length queueMembers + 1)

data AddToQueueManyRes = AddedToQueueMany [(QueueName, Int)] | CurrentlyInGameSome Integer | QueueFilledSome QueueName [BowBotId]

addToQueueMany :: (MonadIOReader m r, Has SafeMysqlConn r) => [(QueueName, Int)] -> BowBotId -> m AddToQueueManyRes
addToQueueMany queues bid = do
  let queueNames = map fst queues
  r <- join <$> queryOnlyLog [mysql|SELECT `current_game` FROM `ranked_bow` WHERE `account_id` = bid|]
  case r of
    Just g -> return $ CurrentlyInGameSome g
    Nothing -> do
      queueMembers <- M.map (map snd) . groupByToMap fst <$> queryLog [mysql|SELECT `queue`, `account_id` FROM `ranked_bow_stats` WHERE `in_queue` AND `queue` IN queueNames|]
      let filledQueues = map fst $ filter (\(n, l) -> let mems = fromMaybe [] $ queueMembers M.!? n in length mems == l - 1 && bid `notElem` mems) queues
      let queuesToFill = filter (\n -> bid `notElem` fromMaybe [] (queueMembers M.!? n)) $ map fst queues
      case filledQueues of
        [] -> do
          let q = map (bid, , True) queuesToFill
          void $ executeLog [mysql|INSERT INTO `ranked_bow_stats`(`account_id`, `queue`, ^`in_queue`) VALUES q..|]
          return $ AddedToQueueMany $ map (\n -> (n, length (fromMaybe [] $ queueMembers M.!? n) + 1)) queuesToFill
        (queue : _) -> do
          let queueMembersAll = bid : fromMaybe [] (queueMembers M.!? queue)
          void $ executeLog [mysql|UPDATE `ranked_bow_stats` SET `in_queue` = 0 WHERE `account_id` IN queueMembersAll|]
          return $ QueueFilledSome queue queueMembersAll
      

removeFromQueue :: (MonadIOReader m r, Has SafeMysqlConn r) => QueueName -> BowBotId -> m (Maybe Int)
removeFromQueue queue bid = do
  c <- (>0) <$> executeLog [mysql|INSERT INTO `ranked_bow_stats`(`account_id`, `queue`, ^`in_queue`) VALUES (bid, queue, 0)|]
  if c then Just . length <$> queryLog [mysql|SELECT `account_id` FROM `ranked_bow_stats` WHERE `queue` = queue AND `in_queue`|] else return Nothing

removeFromAllQueues :: (MonadIOReader m r, Has SafeMysqlConn r) => BowBotId -> m (Maybe [(QueueName, Int)])
removeFromAllQueues bid = do
  c <- (>0) <$> executeLog [mysql|UPDATE `ranked_bow_stats` SET `in_queue` = 0 WHERE `account_id` = bid|]
  if c
    then do
      queueMembers <- M.map (map snd) . groupByToMap fst <$> queryLog [mysql|SELECT `queue`, `account_id` FROM `ranked_bow_stats` WHERE `in_queue`|]
      return $ Just $ M.toList $ M.map length queueMembers
    else return Nothing