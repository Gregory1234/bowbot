{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.BotData.CachedSingle(
  module BowBot.BotData.CachedSingle, MonadIO(..), MonadReader(..), asks, Has(..)
) where

import BowBot.Utils
import Control.Exception.Base (bracket)
import Data.Has


data CacheResponse a
  = CacheBusy
  | CacheFailed
  | CacheFresh a
  | CacheOld a

data CacheResponseDirect a
  = CacheDirectBusy
  | CacheDirectNothing
  | CacheDirectResult a

type HasCachedData a = Has (CachedData a)

getCachedData :: (HasCachedData a r, MonadReader r m) => m (CachedData a)
getCachedData = asks getter

getOrCalculateCacheSingle :: forall a m r. (HasCachedData a r, MonadIO m, MonadReader r m, MonadHoistIO m) => m (Maybe a) -> m (CacheResponse a)
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

getFromCacheSingle :: forall a m r. (HasCachedData a r, MonadIO m, MonadReader r m) => m (CacheResponseDirect a)
getFromCacheSingle = do
  CachedData {..} <- getCachedData @a
  liftIO $ atomically $ do
    busy <- readTVar cachedDataBusy
    if busy then return CacheDirectBusy else do
      main <- readTVar cachedDataMain
      border <- readTVar cachedDataBorder
      return $ maybe CacheDirectNothing CacheDirectResult (main <|> border)
requestCacheSingle :: forall a m r. (HasCachedData a r, MonadIO m, MonadReader r m) => m ()
requestCacheSingle = do
  CachedData {..} <- getCachedData @a
  liftIO $ atomically $ writeTVar cachedDataBusy True
supplyCacheSingle :: forall a m r. (HasCachedData a r, MonadIO m, MonadReader r m) => a -> m ()
supplyCacheSingle a = do
  CachedData {..} <- getCachedData @a
  t <- liftIO $ read @Int <$> getTime "%S"
  liftIO $ atomically $ do
    writeTVar (if t <= 5 || t >= 55 then cachedDataBorder else cachedDataMain) (Just a)
    writeTVar cachedDataBusy False
clearCacheSingle :: forall a m r. (HasCachedData a r, MonadIO m, MonadReader r m) => m ()
clearCacheSingle = do
  CachedData {..} <- getCachedData @a
  liftIO $ atomically $ do
    border <- readTVar cachedDataBorder
    writeTVar cachedDataMain border
    writeTVar cachedDataBorder Nothing
    writeTVar cachedDataBusy False
withCacheSingleBusy :: forall a m r x. (HasCachedData a r, MonadHoistIO m, MonadReader r m) => m x -> m x
withCacheSingleBusy c = do
  CachedData {..} <- getCachedData @a
  hoistIO (bracket (atomically $ writeTVar cachedDataBusy True) (const $ atomically $ writeTVar cachedDataBusy False) . const) c
