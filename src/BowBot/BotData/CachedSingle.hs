{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BowBot.BotData.CachedSingle where

import BowBot.Utils
import Control.Concurrent.STM.TVar (TVar, newTVar)
import Control.Concurrent.STM (STM)
import Control.Applicative ((<|>))
import Control.Exception.Base (bracket)
import Control.Monad.Reader
import Control.Monad.Except


data CacheResponse a
  = CacheBusy
  | CacheFailed
  | CacheFresh a
  | CacheOld a

data CacheResponseDirect a
  = CacheDirectBusy
  | CacheDirectNothing
  | CacheDirectResult a

class MonadIO m => MonadCacheSingle a m where
  getCachedData :: m (CachedData a)

getOrCalculateCacheSingle :: forall a m. (MonadCacheSingle a m, MonadHoistIO m) => m (Maybe a) -> m (CacheResponse a)
getOrCalculateCacheSingle calc = do
  res <- getFromCacheSingle
  case res of
    CacheDirectBusy -> return CacheBusy
    CacheDirectNothing -> do
      r <- withCacheSingleBusy @a calc
      for_ r supplyCacheSingle
      return $ maybe CacheFailed CacheFresh r
    CacheDirectResult a -> return $ CacheOld a

data CachedData a = CachedData { cachedDataMain :: TVar (Maybe a), cachedDataBorder :: TVar (Maybe a), cachedDataBusy :: TVar Bool }

newCachedData :: STM (CachedData a)
newCachedData = CachedData <$> newTVar Nothing <*> newTVar Nothing <*> newTVar False

getFromCacheSingle :: forall a m. MonadCacheSingle a m => m (CacheResponseDirect a)
getFromCacheSingle = do
  CachedData {..} <- getCachedData @a
  liftIO $ atomically $ do
    busy <- readTVar cachedDataBusy
    if busy then return CacheDirectBusy else do
      main <- readTVar cachedDataMain
      border <- readTVar cachedDataBorder
      return $ maybe CacheDirectNothing CacheDirectResult (main <|> border)
requestCacheSingle :: forall a m. MonadCacheSingle a m => m ()
requestCacheSingle = do
  CachedData {..} <- getCachedData @a
  liftIO $ atomically $ writeTVar cachedDataBusy True
supplyCacheSingle :: forall a m. MonadCacheSingle a m => a -> m ()
supplyCacheSingle a = do
  CachedData {..} <- getCachedData @a
  t <- liftIO $ read @Int <$> getTime "%S"
  liftIO $ atomically $ do
    writeTVar (if t <= 5 || t >= 55 then cachedDataBorder else cachedDataMain) (Just a)
    writeTVar cachedDataBusy False
clearCacheSingle :: forall a m. MonadCacheSingle a m => m ()
clearCacheSingle = do
  CachedData {..} <- getCachedData @a
  liftIO $ atomically $ do
    border <- readTVar cachedDataBorder
    writeTVar cachedDataMain border
    writeTVar cachedDataBorder Nothing
    writeTVar cachedDataBusy False
withCacheSingleBusy :: forall a m x. (MonadCacheSingle a m, MonadHoistIO m) => m x -> m x
withCacheSingleBusy c = do
  CachedData {..} <- getCachedData @a
  hoistIO (bracket (atomically $ writeTVar cachedDataBusy True) (const $ atomically $ writeTVar cachedDataBusy False) . const) c


instance MonadCacheSingle a m => MonadCacheSingle a (ReaderT r m) where
  getCachedData = ReaderT $ const $ getCachedData @a

instance MonadCacheSingle a m => MonadCacheSingle a (ExceptT e m) where
  getCachedData = ExceptT $ Right <$> getCachedData @a