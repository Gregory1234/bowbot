{-# LANGUAGE MultiWayIf #-}
module BowBot.Ranked.Queue where

import Control.Concurrent.MVar
import BowBot.Discord.Utils

data GameQueue = GameQueue { queueMembers :: MVar [UserId], queueCapacity :: Int }

data AddToQueueRes = AddedToQueue | AlreadyInQueue | QueueFilled [UserId]

addToQueue :: (MonadIOReader m r, Has GameQueue r) => UserId -> m AddToQueueRes
addToQueue did = do
  GameQueue {..} <- asks getter
  q <- liftIO $ takeMVar queueMembers
  let newQ = did : q
  if
    | did `elem` q -> return AlreadyInQueue
    | length newQ == queueCapacity -> do
        liftIO $ putMVar queueMembers []
        return $ QueueFilled newQ
    | otherwise -> do
        liftIO $ putMVar queueMembers newQ
        return AddedToQueue

removeFromQueue :: (MonadIOReader m r, Has GameQueue r) => UserId -> m Bool
removeFromQueue did = asks getter >>= \GameQueue {..} -> liftIO $ do
  q <- takeMVar queueMembers
  if did `elem` q
    then do
      putMVar queueMembers q
      return False
    else do
      putMVar queueMembers (filter (/= did) q)
      return True

emptyGameQueue :: MonadIO m => Int -> m GameQueue
emptyGameQueue queueCapacity = liftIO $ (\queueMembers -> GameQueue {..}) <$> newMVar []