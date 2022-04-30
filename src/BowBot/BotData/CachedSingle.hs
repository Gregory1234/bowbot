{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module BowBot.BotData.CachedSingle where

import BowBot.Utils
import Data.Proxy
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
  getFromCacheSingle :: proxy a -> m (CacheResponseDirect a)
  requestCacheSingle :: proxy a -> m ()
  supplyCacheSingle :: a -> m ()
  clearCacheSingle :: proxy a -> m ()
  withCacheSingleBusy :: proxy a -> m x -> m x

getOrCalculateCacheSingle :: MonadCacheSingle a m => m (Maybe a) -> m (CacheResponse a)
getOrCalculateCacheSingle calc = do
  res <- getFromCacheSingle Proxy
  case res of
    CacheDirectBusy -> return CacheBusy
    CacheDirectNothing -> do
      r <- withCacheSingleBusy res calc
      for_ r supplyCacheSingle
      return $ maybe CacheFailed CacheFresh r
    CacheDirectResult a -> return $ CacheOld a

data CachedData a = CachedData { cachedDataMain :: TVar (Maybe a), cachedDataBorder :: TVar (Maybe a), cachedDataBusy :: TVar Bool }

newCachedData :: STM (CachedData a)
newCachedData = CachedData <$> newTVar Nothing <*> newTVar Nothing <*> newTVar False

class MonadIO m => MonadSimpleCacheSingle a m where
  getCachedData :: proxy a -> m (CachedData a)

newtype SimpleCacheSingle m a = SimpleCacheSingle { unSimpleCacheSingle :: m a } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadHoistIO, MonadSimpleCacheSingle c)

instance (MonadHoistIO m, MonadSimpleCacheSingle a m) => MonadCacheSingle a (SimpleCacheSingle m) where
  getFromCacheSingle proxy = do
    CachedData {..} <- getCachedData proxy
    liftIO $ atomically $ do
      busy <- readTVar cachedDataBusy
      if busy then return CacheDirectBusy else do
        main <- readTVar cachedDataMain
        border <- readTVar cachedDataBorder
        return $ maybe CacheDirectNothing CacheDirectResult (main <|> border)
  requestCacheSingle proxy = do
    CachedData {..} <- getCachedData proxy
    liftIO $ atomically $ writeTVar cachedDataBusy True
  supplyCacheSingle a = do
    CachedData {..} <- getCachedData Proxy
    t <- liftIO $ read @Int <$> getTime "%S"
    liftIO $ atomically $ do
      writeTVar (if t <= 5 || t >= 55 then cachedDataBorder else cachedDataMain) (Just a)
      writeTVar cachedDataBusy False
  clearCacheSingle proxy = do
    CachedData {..} <- getCachedData proxy
    liftIO $ atomically $ do
      border <- readTVar cachedDataBorder
      writeTVar cachedDataMain border
      writeTVar cachedDataBorder Nothing
      writeTVar cachedDataBusy False
  withCacheSingleBusy proxy c = do
    CachedData {..} <- getCachedData proxy
    hoistIO (bracket (atomically $ writeTVar cachedDataBusy True) (const $ atomically $ writeTVar cachedDataBusy False) . const) c


instance MonadCacheSingle c m => MonadCacheSingle c (ReaderT r m) where
  getFromCacheSingle proxy = ReaderT $ const $ getFromCacheSingle proxy
  requestCacheSingle proxy = ReaderT $ const $ requestCacheSingle proxy
  supplyCacheSingle a = ReaderT $ const $ supplyCacheSingle a
  clearCacheSingle proxy = ReaderT $ const $ clearCacheSingle proxy
  withCacheSingleBusy proxy (ReaderT f) = ReaderT $ withCacheSingleBusy proxy . f

instance MonadCacheSingle c m => MonadCacheSingle c (ExceptT e m) where
  getFromCacheSingle proxy = ExceptT $ Right <$> getFromCacheSingle proxy
  requestCacheSingle proxy = ExceptT $ Right <$> requestCacheSingle proxy
  supplyCacheSingle a = ExceptT $ Right <$> supplyCacheSingle a
  clearCacheSingle proxy = ExceptT $ Right <$> clearCacheSingle proxy
  withCacheSingleBusy proxy (ExceptT x) = ExceptT $ withCacheSingleBusy proxy x