{-# LANGUAGE QuasiQuotes #-}

module BowBot.Ranked.Queue where

import BowBot.Discord.Utils
import BowBot.DB.Basic
import BowBot.Account.Basic
import BowBot.BotData.Info
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Bifunctor (first)

newtype QueueName = QueueName { queueName :: Text }
  deriving newtype (Show, Eq, Ord, ToMysqlSimple, FromMysqlSimple, ToMysql, FromMysql)

rankedModRoleInfo :: InfoType RoleId
rankedModRoleInfo = InfoType { infoName = "ranked_mod_role", infoDefault = 0, infoParse = first pack . readEither . unpack }

rankedBowQueuesInfo :: InfoType [(QueueName, [Text])]
rankedBowQueuesInfo = InfoType { infoName = "ranked_bow_queues", infoDefault = [], infoParse = \s -> for (T.lines s) $ \l -> case T.splitOn "->" l of [a, b] -> Right (QueueName a, T.splitOn "," b); _ -> Left "wrong format" }

getQueueByName :: (MonadIOReader m r, Has InfoCache r) => Text -> m (Maybe QueueName)
getQueueByName (T.toLower -> name) = do
  rankedBowQueues <- askInfo rankedBowQueuesInfo
  return $ if QueueName name `elem` map fst rankedBowQueues then Just $ QueueName name else Nothing

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
      void $ executeLog [mysql|INSERT INTO `ranked_bow_stats`(`account_id`, `queue`, ^`in_queue`) VALUES (bid, queue, 1)|]
      queueMembers <- queryLog [mysql|SELECT `account_id` FROM `ranked_bow_stats` WHERE `queue` = queue AND `in_queue`|]
      if length queueMembers == limit
        then do
          void $ executeLog [mysql|UPDATE `ranked_bow_stats` SET `in_queue` = 0 WHERE `account_id` IN queueMembers|]
          return $ Just $ QueueFilled queueMembers
        else return $ Just $ AddedToQueue $ length queueMembers

data AddToQueueManyRes = AddedToQueueMany [(QueueName, Int)] | CurrentlyInGameSome Integer | QueueFilledSome QueueName [BowBotId]

addToQueueMany :: (MonadIOReader m r, Has SafeMysqlConn r) => [(QueueName, Int)] -> BowBotId -> m AddToQueueManyRes
addToQueueMany queues bid = do
  let queueNames = map fst queues
  r <- join <$> queryOnlyLog [mysql|SELECT `current_game` FROM `ranked_bow` WHERE `account_id` = bid|]
  case r of
    Just g -> return $ CurrentlyInGameSome g
    Nothing -> do
      let q = map ((bid, , True) . fst) queues
      void $ executeLog [mysql|INSERT INTO `ranked_bow_stats`(`account_id`, `queue`, ^`in_queue`) VALUES q..|]

      queueMembers <- M.map (map snd) . groupByToMap fst <$> queryLog [mysql|SELECT `queue`, `account_id` FROM `ranked_bow_stats` WHERE `in_queue` AND `queue` IN queueNames|]
      let filledQueues = map fst $ filter (\(n, l) -> maybe 0 length (queueMembers M.!? n) == l) queues
      
      case filledQueues of
        [] -> return $ AddedToQueueMany $ map (\(n, _) -> (n, length (fromMaybe [] $ queueMembers M.!? n))) queues
        (queue : _) -> do
          let queueMembersThisQueue = fromMaybe [] (queueMembers M.!? queue)
          void $ executeLog [mysql|UPDATE `ranked_bow_stats` SET `in_queue` = 0 WHERE `account_id` IN queueMembersThisQueue|]
          return $ QueueFilledSome queue queueMembersThisQueue
      

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