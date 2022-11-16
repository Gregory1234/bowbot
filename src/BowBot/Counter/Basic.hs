{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.Counter.Basic where

import BowBot.Utils
import qualified Data.Map as M

data Counter = Counter { counterMain :: TVar Integer, counterBorder :: TVar Integer }

class Counted c where
  counterLimit :: c -> Integer
  counterName :: c -> String

newtype CounterState = CounterState { counterState :: TVar (M.Map String Counter) }

getCounter :: (MonadIOReader m r, Has CounterState r, Counted c) => c -> m Counter
getCounter c = do
  stvar <- asks (counterState . getter)
  liftIO $ atomically $ do
    st <- readTVar stvar
    case st M.!? counterName c of
      Just counter -> return counter
      Nothing -> do
        counter <- newCounter
        modifyTVar stvar (M.insert (counterName c) counter)
        return counter

getCurrentCounterValue :: (MonadIOReader m r, Has CounterState r, Counted c) => c -> m Integer
getCurrentCounterValue c = do
  Counter {..} <- getCounter c
  liftIO $ atomically $ do
    c1 <- readTVar counterMain
    c2 <- readTVar counterBorder
    return $ c1 + c2
tryIncreaseCounter :: (MonadIOReader m r, Has CounterState r, Counted c) => c -> Integer -> m (Maybe Int)
tryIncreaseCounter c extra = do
  Counter {..} <- getCounter c
  t <- liftIO $ read @Int <$> getTime "%S"
  cv <- liftIO $ atomically $ do
    c1 <- readTVar counterMain
    c2 <- readTVar counterBorder
    let c' = c1 + c2 + extra
    when (c' <= counterLimit c) $ modifyTVar (if t <= 5 || t >= 55 then counterBorder else counterMain) (+ extra)
    return $ c' <= counterLimit c
  return $ if cv then Nothing else Just ((65 - t) `mod` 60)
clearCounter :: (MonadIOReader m r, Has CounterState r, Counted c) => c -> m ()
clearCounter c = do
  Counter {..} <- getCounter c
  liftIO $ atomically $ do
    border <- readTVar counterBorder
    writeTVar counterMain border
    writeTVar counterBorder 0

newCounter :: STM Counter
newCounter = Counter <$> newTVar 0 <*> newTVar 0

newCounterState :: STM CounterState
newCounterState = CounterState <$> newTVar M.empty