{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module BowBot.BotData.CachedSingle(
  module BowBot.BotData.CachedSingle, MonadIO(..), MonadReader(..), asks, Has(..), module BowBot.BotData.HasData
) where

import BowBot.Utils
import Control.Exception.Base (bracket)
import BowBot.BotData.HasData


data CacheResponse a
  = CacheBusy
  | CacheFailed
  | CacheFresh a
  | CacheOld a

cacheResponseToMaybe :: CacheResponse a -> Maybe a
cacheResponseToMaybe CacheBusy = Nothing
cacheResponseToMaybe CacheFailed = Nothing
cacheResponseToMaybe (CacheFresh a) = Just a
cacheResponseToMaybe (CacheOld a) = Just a

data CacheResponseDirect a
  = CacheDirectBusy
  | CacheDirectNothing
  | CacheDirectResult a

type HasCachedData a = Has (CachedData a)

getCachedData :: (HasCachedData a d, MonadReader r m, HasBotData d r) => m (CachedData a)
getCachedData = asks getterData

getOrCalculateCacheSingle :: forall a m d r. (HasCachedData a d, MonadHoistIOBotData m d r) => m (Maybe a) -> m (CacheResponse a)
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

getFromCacheSingle :: forall a m d r. (HasCachedData a d, MonadIOBotData m d r) => m (CacheResponseDirect a)
getFromCacheSingle = do
  CachedData {..} <- getCachedData @a
  liftIO $ atomically $ do
    busy <- readTVar cachedDataBusy
    if busy then return CacheDirectBusy else do
      main <- readTVar cachedDataMain
      border <- readTVar cachedDataBorder
      return $ maybe CacheDirectNothing CacheDirectResult (main <|> border)
requestCacheSingle :: forall a m d r. (HasCachedData a d, MonadIOBotData m d r) => m ()
requestCacheSingle = do
  CachedData {..} <- getCachedData @a
  liftIO $ atomically $ writeTVar cachedDataBusy True
supplyCacheSingle :: forall a m d r. (HasCachedData a d, MonadIOBotData m d r) => a -> m ()
supplyCacheSingle a = do
  CachedData {..} <- getCachedData @a
  t <- liftIO $ read @Int <$> getTime "%S"
  liftIO $ atomically $ do
    writeTVar (if t <= 5 || t >= 55 then cachedDataBorder else cachedDataMain) (Just a)
    writeTVar cachedDataBusy False
clearCacheSingle :: forall a m d r. (HasCachedData a d, MonadIOBotData m d r) => m ()
clearCacheSingle = do
  CachedData {..} <- getCachedData @a
  liftIO $ atomically $ do
    border <- readTVar cachedDataBorder
    writeTVar cachedDataMain border
    writeTVar cachedDataBorder Nothing
    writeTVar cachedDataBusy False
withCacheSingleBusy :: forall a m d r x. (HasCachedData a d, MonadHoistIOBotData m d r) => m x -> m x
withCacheSingleBusy c = do
  CachedData {..} <- getCachedData @a
  hoistIO (bracket (atomically $ writeTVar cachedDataBusy True) (const $ atomically $ writeTVar cachedDataBusy False) . const) c
