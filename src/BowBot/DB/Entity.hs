{-# LANGUAGE TypeFamilies #-}

module BowBot.DB.Entity where

import Database.MySQL.Simple (Connection)
import BowBot.DB.Class
import Control.Monad.IO.Class (liftIO)


class DBEntity a where
  type DBUniqueKey a
  data DBFilter a
  emptyFilter :: DBFilter a
  uniqueKey :: a -> DBUniqueKey a
  getFromDB :: Connection -> proxy a -> DBUniqueKey a -> IO (Maybe a)
  filterFromDB :: Connection -> DBFilter a -> IO [a]
  storeInDB :: Connection -> [a] -> IO Bool

allFromDB :: DBEntity a => Connection -> proxy a -> IO [a]
allFromDB conn _ = filterFromDB conn emptyFilter

hAllFromDB :: (MonadDB m, DBEntity a) => proxy a -> m [a]
hAllFromDB proxy = do
  c <- hConnection
  liftIO $ allFromDB c proxy

hGetFromDB :: (MonadDB m, DBEntity a) => proxy a -> DBUniqueKey a -> m (Maybe a)
hGetFromDB proxy k = do
  c <- hConnection
  liftIO $ getFromDB c proxy k

hFilterFromDB :: (MonadDB m, DBEntity a) => DBFilter a -> m [a]
hFilterFromDB f = do
  c <- hConnection
  liftIO $ filterFromDB c f

hStoreInDB :: (MonadDB m, DBEntity a) => [a] -> m Bool
hStoreInDB v = do
  c <- hConnection
  liftIO $ storeInDB c v

