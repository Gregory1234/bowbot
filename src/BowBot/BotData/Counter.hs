{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}

module BowBot.BotData.Counter(
  module BowBot.BotData.Counter, MonadIO(..), MonadReader(..), asks, Has(..)
) where

import BowBot.Utils
import Data.Has

class Counted c where
  counterLimit :: c -> Integer

type HasCounter c r = (Has (Counter c) r, Counted c)
type HasCounter' c = Has (Counter c)

getCounter :: (HasCounter c r, MonadReader r m) => c -> m (Counter c)
getCounter = const $ asks getter

getCurrentCounterValue :: (HasCounter c r, MonadIO m, MonadReader r m) => c -> m Integer
getCurrentCounterValue c = do
  Counter {..} <- getCounter c
  liftIO $ atomically $ do
    c1 <- readTVar counterMain
    c2 <- readTVar counterBorder
    return $ c1 + c2
tryIncreaseCounter :: (HasCounter c r, MonadIO m, MonadReader r m) => c -> Integer -> m (Maybe Int)
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
clearCounter :: (HasCounter c r, MonadIO m, MonadReader r m) => c -> m ()
clearCounter c = do
  Counter {..} <- getCounter c
  liftIO $ atomically $ do
    border <- readTVar counterBorder
    writeTVar counterMain border
    writeTVar counterBorder 0

data Counter c = Counter { counterMain :: TVar Integer, counterBorder :: TVar Integer }

newCounter :: STM (Counter c)
newCounter = Counter <$> newTVar 0 <*> newTVar 0