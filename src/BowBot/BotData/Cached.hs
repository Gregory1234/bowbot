{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module BowBot.BotData.Cached where

import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Control.Concurrent.STM.TVar (TVar)
import Database.MySQL.Simple (Connection)
import BowBot.Utils (readTVar, atomically, assertIO)
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Except
import Data.Kind (Constraint, Type)

type DatabaseCache a = TVar (HM.HashMap (CacheIndex a) a)

class MonadIO m => MonadCache a m where
  getCache :: proxy a -> m (DatabaseCache a)

class (Eq (CacheIndex a), Hashable (CacheIndex a)) => Cached a where
  type CacheIndex a
  refreshCache :: MonadCache a m => Connection -> proxy a -> m ()

class Cached a => CachedStorable a where
  storeInCacheIndexed :: MonadCache a m => [(CacheIndex a, a)] -> m Bool

class Cached a => CachedIndexed a where
  cacheIndex :: a -> CacheIndex a
  storeInCache :: MonadCache a m => [a] -> m Bool

class Cached a => CachedUpdatable a where
  type CacheUpdateSourceConstraint a :: (Type -> Type) -> Constraint
  updateCache :: (CacheUpdateSourceConstraint a m, MonadCache a m) => proxy a -> m ()

getFromCache :: (MonadCache a m, Cached a) => proxy a -> CacheIndex a -> m (Maybe a)
getFromCache proxy a = do
  m <- getCacheMap proxy
  return $ m HM.!? a

getCacheMap :: MonadCache a m => proxy a -> m (HM.HashMap (CacheIndex a) a)
getCacheMap proxy = do
  cache <- getCache proxy
  liftIO $ atomically $ readTVar cache

insertMany :: (Eq a, Hashable a) => [(a,b)] -> HM.HashMap a b -> HM.HashMap a b
insertMany [] m = m
insertMany ((a,b):as) m = HM.insert a b $ insertMany as m

assertGoodIndexes :: (CachedIndexed a, MonadIO m) => [(CacheIndex a, a)] -> m ()
assertGoodIndexes [] = pure ()
assertGoodIndexes ((a,b):as) = do
  assertIO (a == cacheIndex b)
  assertGoodIndexes as

instance MonadCache c m => MonadCache c (ReaderT r m) where
  getCache proxy = ReaderT $ const $ getCache proxy

instance MonadCache c m => MonadCache c (ExceptT e m) where
  getCache proxy = ExceptT $ Right <$> getCache proxy