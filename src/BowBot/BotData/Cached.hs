{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}

module BowBot.BotData.Cached(
  module BowBot.BotData.Cached, MonadIO(..), MonadReader(..), asks, Has(..)
) where

import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Database.MySQL.Simple (Connection)
import BowBot.Utils
import Data.Has

newtype DatabaseCache a = DatabaseCache { unDatabaseCache :: TVar (HM.HashMap (CacheIndex a) a) }

newCache :: STM (DatabaseCache a)
newCache = DatabaseCache <$> newTVar HM.empty

type HasCache a = Has (DatabaseCache a)

getCache :: (HasCache a r, MonadReader r m) => m (TVar (HM.HashMap (CacheIndex a) a))
getCache = asks (unDatabaseCache . getter)

class (Eq (CacheIndex a), Hashable (CacheIndex a)) => Cached a where
  type CacheIndex a
  refreshCache :: (HasCache a r, MonadIO m, MonadReader r m) => Connection -> m ()

class Cached a => CachedStorable a where
  storeInCacheIndexed :: (HasCache a r, MonadIO m, MonadReader r m) => [(CacheIndex a, a)] -> m Bool

class Cached a => CachedIndexed a where
  cacheIndex :: a -> CacheIndex a
  storeInCache :: (HasCache a r, MonadIO m, MonadReader r m) => [a] -> m Bool

getFromCache :: (HasCache a r, MonadIO m, MonadReader r m, Cached a) => CacheIndex a -> m (Maybe a)
getFromCache a = do
  m <- getCacheMap
  return $ m HM.!? a

getCacheMap :: (HasCache a r, MonadIO m, MonadReader r m) => m (HM.HashMap (CacheIndex a) a)
getCacheMap = do
  cache <- getCache
  liftIO $ atomically $ readTVar cache

insertMany :: (Eq a, Hashable a) => [(a,b)] -> HM.HashMap a b -> HM.HashMap a b
insertMany [] m = m
insertMany ((a,b):as) m = HM.insert a b $ insertMany as m

assertGoodIndexes :: (CachedIndexed a, MonadIO m) => [(CacheIndex a, a)] -> m ()
assertGoodIndexes [] = pure ()
assertGoodIndexes ((a,b):as) = do
  assertIO (a == cacheIndex b)
  assertGoodIndexes as