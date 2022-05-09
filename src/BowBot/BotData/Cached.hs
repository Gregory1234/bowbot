{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module BowBot.BotData.Cached where

import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Control.Concurrent.STM.TVar (TVar)
import Database.MySQL.Simple (Connection)
import BowBot.Utils (readTVar, atomically, assertIO)
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Except

type DatabaseCache a = TVar (HM.HashMap (CacheIndex a) a)

class MonadIO m => MonadCache a m where
  getCache :: m (DatabaseCache a)

class (Eq (CacheIndex a), Hashable (CacheIndex a)) => Cached a where
  type CacheIndex a
  refreshCache :: MonadCache a m => Connection -> m ()

class Cached a => CachedStorable a where
  storeInCacheIndexed :: MonadCache a m => [(CacheIndex a, a)] -> m Bool

class Cached a => CachedIndexed a where
  cacheIndex :: a -> CacheIndex a
  storeInCache :: MonadCache a m => [a] -> m Bool

getFromCache :: (MonadCache a m, Cached a) => CacheIndex a -> m (Maybe a)
getFromCache a = do
  m <- getCacheMap
  return $ m HM.!? a

getCacheMap :: MonadCache a m => m (HM.HashMap (CacheIndex a) a)
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

instance MonadCache c m => MonadCache c (ReaderT r m) where
  getCache = ReaderT $ const getCache

instance MonadCache c m => MonadCache c (ExceptT e m) where
  getCache = ExceptT $ Right <$> getCache