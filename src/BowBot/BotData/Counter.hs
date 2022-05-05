{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BowBot.BotData.Counter where

import BowBot.Utils
import Control.Concurrent.STM.TVar (TVar, newTVar)
import Control.Concurrent.STM (STM)
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Except

class Counted c where
  counterLimit :: Integer

class (Counted c, MonadIO m) => MonadCounter c m where
  getCurrentCounterValue :: m Integer
  tryIncreaseCounter :: Integer -> m (Maybe Int)
  clearCounter :: m ()

data Counter = Counter { counterMain :: TVar Integer, counterBorder :: TVar Integer }

newCounter :: STM Counter
newCounter = Counter <$> newTVar 0 <*> newTVar 0

class (Counted c, MonadIO m) => MonadSimpleCounter c m where
  getCounter :: m Counter

newtype SimpleCounter m a = SimpleCounter { unSimpleCounter :: m a } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadSimpleCounter c)

instance (Counted c, MonadIO m, MonadSimpleCounter c m) => MonadCounter c (SimpleCounter m) where
  getCurrentCounterValue = do
    Counter {..} <- getCounter @c
    liftIO $ atomically $ do
      c1 <- readTVar counterMain
      c2 <- readTVar counterBorder
      return $ c1 + c2
  tryIncreaseCounter extra = do
    Counter {..} <- getCounter @c
    t <- liftIO $ read @Int <$> getTime "%S"
    cv <- liftIO $ atomically $ do
      c1 <- readTVar counterMain
      c2 <- readTVar counterBorder
      let c = c1 + c2 + extra
      when (c <= counterLimit @c) $ modifyTVar (if t <= 5 || t >= 55 then counterBorder else counterMain) (+ extra)
      return $ c <= counterLimit @c
    return $ if cv then Nothing else Just ((65 - t) `mod` 60)
  clearCounter = do
    Counter {..} <- getCounter @c
    liftIO $ atomically $ do
      border <- readTVar counterBorder
      writeTVar counterMain border
      writeTVar counterBorder 0

instance MonadCounter c m => MonadCounter c (ReaderT r m) where
  getCurrentCounterValue = ReaderT $ const $ getCurrentCounterValue @c
  tryIncreaseCounter extra = ReaderT $ const $ tryIncreaseCounter @c extra
  clearCounter = ReaderT $ const $ clearCounter @c

instance MonadCounter c m => MonadCounter c (ExceptT e m) where
  getCurrentCounterValue = ExceptT $ Right <$> getCurrentCounterValue @c
  tryIncreaseCounter extra = ExceptT $ Right <$> tryIncreaseCounter @c extra
  clearCounter = ExceptT $ Right <$> clearCounter @c