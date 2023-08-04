{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module BowBot.DB.Typed(
  module BowBot.DB.Typed, module BowBot.DB.Basic, module Language.MySQL.Quasi
) where

import BowBot.Utils
import qualified Database.MySQL.Simple.Types as Q
import BowBot.DB.Basic
import Data.Int (Int64)
import Language.MySQL.Query
import Language.MySQL.Quasi

queryLogT :: (FromMysql r, MonadIOReader m rd, Has Connection rd) => (Connection -> IO (Query r)) -> m [r]
queryLogT q = do
  conn <- asks getter
  (Query q') <- liftIO $ q conn
  queryLog_ (Q.Query q')

queryOnlyLogT :: (FromMysql r, MonadIOReader m rd, Has Connection rd) => (Connection -> IO (Query r)) -> m (Maybe r)
queryOnlyLogT = fmap only . queryLogT

executeLogT :: (MonadIOReader m r, Has Connection r) => (Connection -> IO Command) -> m Int64
executeLogT q = do
  conn <- asks getter
  (Command q') <- liftIO $ q conn
  case q' of
    Nothing -> 0 <$ logInfo "Tried executing query with no data!" -- TODO: provide more info
    Just q'' -> executeLog_ (Q.Query q'')
