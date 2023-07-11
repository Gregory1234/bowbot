{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module BowBot.DB.Typed(
  module BowBot.DB.Typed, module BowBot.DB.Basic, module BowBot.DB.Typed.DatabaseTable, module BowBot.DB.Typed.Query
) where

import BowBot.Utils
import BowBot.DB.Typed.Query
import BowBot.DB.Typed.DatabaseTable
import BowBot.DB.Typed.Types
import BowBot.DB.Basic
import Data.Int (Int64)

queryLogT :: (QueryParams q, QueryResults r, MonadIOReader m rd, Has Connection rd) => TypedQuery q r -> q -> m [r]
queryLogT = queryLog . fromTypedQuery

queryLogT_ :: (QueryResults r, MonadIOReader m rd, Has Connection rd) => TypedQuery () r -> m [r]
queryLogT_ = queryLog_ . fromTypedQuery

queryOnlyLogT :: (QueryParams q, QueryResults r, MonadIOReader m rd, Has Connection rd) => TypedQuery q r -> q -> m (Maybe r)
queryOnlyLogT = queryOnlyLog . fromTypedQuery

executeLogT :: (QueryParams q, MonadIOReader m r, Has Connection r) => TypedCommand q -> q -> m Int64
executeLogT = executeLog . fromTypedCommand

executeLogT_ :: (MonadIOReader m r, Has Connection r) => TypedCommand () -> m Int64
executeLogT_ = executeLog_ . fromTypedCommand

executeManyLogT :: (QueryParams q, MonadIOReader m r, Has Connection r) => TypedCommand q -> [q] -> m Int64
executeManyLogT = executeManyLog . fromTypedCommand
