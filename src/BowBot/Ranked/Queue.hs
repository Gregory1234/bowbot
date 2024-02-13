{-# LANGUAGE QuasiQuotes #-}

module BowBot.Ranked.Queue where

import BowBot.Discord.Utils
import BowBot.DB.Basic
import BowBot.Account.Basic
import Control.Monad.Except


data AddToQueueRes = AddedToQueue Int | AlreadyInQueue | CurrentlyInGame Integer | QueueFilled [BowBotId]

addToQueue :: (MonadIOReader m r, Has SafeMysqlConn r) => Int -> BowBotId -> m (Maybe AddToQueueRes)
addToQueue limit bid = do
  ctx <- ask
  liftIO $ (`runReaderT` ctx) $ withTransaction $ (either (const $ rollback $> Nothing) (pure . Just) =<<) $ runExceptT $ do
    (inQueue, currentGame) <- liftMaybe () =<< queryOnlyLog [mysql|SELECT `queue`,`current_game` FROM `ranked_bow_stats` WHERE `account_id` = bid|]
    case (inQueue, currentGame) of
      (False, Just g) -> return $ CurrentlyInGame g
      (True, _) -> return AlreadyInQueue
      (False, Nothing) -> do
        queue <- queryLog [mysql|SELECT `account_id` FROM `ranked_bow_stats` WHERE `queue`|]
        if length queue == limit - 1
          then do
            void $ executeLog [mysql|UPDATE `ranked_bow_stats` SET `queue` = 0|]
            return $ QueueFilled (bid : queue)
          else do
            void $ executeLog [mysql|INSERT INTO `ranked_bow_stats`(`account_id`, ^`queue`) VALUES (bid, 1)|]
            return $ AddedToQueue (length queue + 1)

removeFromQueue :: (MonadIOReader m r, Has SafeMysqlConn r) => BowBotId -> m (Maybe Int)
removeFromQueue bid = do
  c <- (>0) <$> executeLog [mysql|INSERT INTO `ranked_bow_stats`(`account_id`, ^`queue`) VALUES (bid, 0)|]
  if c then Just . length <$> queryLog [mysql|SELECT `account_id` FROM `ranked_bow_stats` WHERE `queue`|] else return Nothing