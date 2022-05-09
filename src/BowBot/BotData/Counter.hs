{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module BowBot.BotData.Counter where

import BowBot.Utils
import Control.Concurrent.STM.TVar (TVar, newTVar)
import Control.Concurrent.STM (STM)
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Except

class Counted c where
  counterLimit :: c -> Integer

class (Counted c, MonadIO m) => MonadCounter c m where
  getCounter :: c -> m (Counter c)

getCurrentCounterValue :: MonadCounter c m => c -> m Integer
getCurrentCounterValue c = do
  Counter {..} <- getCounter c
  liftIO $ atomically $ do
    c1 <- readTVar counterMain
    c2 <- readTVar counterBorder
    return $ c1 + c2
tryIncreaseCounter :: MonadCounter c m => c -> Integer -> m (Maybe Int)
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
clearCounter :: MonadCounter c m => c -> m ()
clearCounter c = do
  Counter {..} <- getCounter c
  liftIO $ atomically $ do
    border <- readTVar counterBorder
    writeTVar counterMain border
    writeTVar counterBorder 0

data Counter c = Counter { counterMain :: TVar Integer, counterBorder :: TVar Integer }

newCounter :: STM (Counter c)
newCounter = Counter <$> newTVar 0 <*> newTVar 0

instance MonadCounter c m => MonadCounter c (ReaderT r m) where
  getCounter c = ReaderT $ const $ getCounter c

instance MonadCounter c m => MonadCounter c (ExceptT e m) where
  getCounter c = ExceptT $ Right <$> getCounter c