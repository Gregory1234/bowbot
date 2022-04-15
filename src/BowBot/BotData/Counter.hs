{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module BowBot.BotData.Counter where

import BowBot.Utils
import Control.Concurrent.STM.TVar (TVar, newTVar)
import Control.Concurrent.STM (STM)
import Control.Monad.Reader (ReaderT(..))

class Counted c where
  counterLimit :: proxy c -> Integer

class (Counted c, MonadIO m) => MonadCounter c m where
  getCurrentCounterValue :: proxy c -> m Integer
  tryIncreaseCounter :: proxy c -> Integer -> m (Maybe Int)
  clearCounter :: proxy c -> m ()

instance MonadCounter c m => MonadCounter c (ReaderT r m) where
  getCurrentCounterValue proxy = ReaderT $ const $ getCurrentCounterValue proxy
  tryIncreaseCounter proxy extra = ReaderT $ const $ tryIncreaseCounter proxy extra
  clearCounter proxy = ReaderT $ const $ clearCounter proxy

data Counter = Counter { counterMain :: TVar Integer, counterBorder :: TVar Integer }

newCounter :: STM Counter
newCounter = Counter <$> newTVar 0 <*> newTVar 0

class (Counted c, MonadIO m) => MonadSimpleCounter c m where
  getCounter :: proxy c -> m Counter

newtype SimpleCounter m a = SimpleCounter { unSimpleCounter :: m a } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadSimpleCounter c)

instance (Counted c, MonadIO m, MonadSimpleCounter c m) => MonadCounter c (SimpleCounter m) where
  getCurrentCounterValue proxy = do
    Counter {..} <- getCounter proxy
    liftIO $ atomically $ do
      c1 <- readTVar counterMain
      c2 <- readTVar counterBorder
      return $ c1 + c2
  tryIncreaseCounter proxy extra = do
    Counter {..} <- getCounter proxy
    t <- liftIO $ read @Int <$> getTime "%S"
    cv <- liftIO $ atomically $ do
      c1 <- readTVar counterMain
      c2 <- readTVar counterBorder
      let c = c1 + c2 + extra
      when (c <= counterLimit proxy) $ modifyTVar (if t <= 5 || t >= 55 then counterBorder else counterMain) (+ extra)
      return $ c <= counterLimit proxy
    return $ if cv then Nothing else Just ((65 - t) `mod` 60)
  clearCounter proxy = do
    Counter {..} <- getCounter proxy
    liftIO $ atomically $ do
      border <- readTVar counterBorder
      writeTVar counterMain border
      writeTVar counterBorder 0