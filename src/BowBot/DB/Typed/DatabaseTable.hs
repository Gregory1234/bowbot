{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module BowBot.DB.Typed.DatabaseTable where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Kind (Type, Constraint)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Data (Proxy(..))
import Data.Coerce

data ColumnTableName = ColumnTableName { columnTableNameString :: ByteString, columnTableNameExplicit :: Bool }

class DatabaseTableLike t where
  columnName :: t a -> ByteString
  columnTableName :: t a -> ColumnTableName
  default columnTableName :: (DatabaseTable t) => t a -> ColumnTableName
  columnTableName _ = ColumnTableName { columnTableNameString = tableName @t, columnTableNameExplicit = False }

type SomeCol :: (Type -> Type) -> Type
data SomeCol t = forall a. SomeCol { getSomeCol :: t a }

columnName' :: DatabaseTableLike t => SomeCol t -> ByteString
columnName' (SomeCol a) = columnName a

columnTableName' :: DatabaseTableLike t => SomeCol t -> ColumnTableName
columnTableName' (SomeCol a) = columnTableName a

type DatabaseTable :: (Type -> Type) -> Constraint
class (InTable t (PrimaryKey t), DatabaseTableLike t, MainTable (PrimaryKey t) ~ t) => DatabaseTable t where
  data PrimaryKey t :: Type
  tableName :: ByteString

newtype ColRep t a = ColRep { toSomeCols :: [SomeCol t] }

type InTable :: (Type -> Type) -> Type -> Constraint
class (DatabaseTableLike t) => InTable t a where
  columnRep :: ColRep t a

someCols :: forall t a. InTable t a => [SomeCol t]
someCols = toSomeCols $ columnRep @t @a

type MainTable :: Type -> Type -> Type
type family MainTable a

instance (InTable t a, InTable t b) => InTable t (a,b) where
  columnRep = ColRep $ someCols @t @a ++ someCols @t @b

type instance MainTable (a,b) = MainTable a

someCols' :: forall a. InTable (MainTable a) a => [SomeCol (MainTable a)]
someCols' = someCols @(MainTable a) @a

data JoinTable t s a where
  JoinTLeft :: t a -> JoinTable t s a
  JoinTRight :: s a -> JoinTable t s a

instance (DatabaseTableLike t, DatabaseTableLike s) => DatabaseTableLike (JoinTable t s) where
  columnName (JoinTLeft c) = columnName c
  columnName (JoinTRight c) = columnName c
  columnTableName (JoinTLeft c) = (columnTableName c) { columnTableNameExplicit = True }
  columnTableName (JoinTRight c) = (columnTableName c) { columnTableNameExplicit = True }

newtype RenamedTable (s :: Symbol) t a = RenamedTable (t a)

renamedTableAlias :: forall t s t'. (t ~ RenamedTable s t', KnownSymbol s) => ByteString
renamedTableAlias = BS.pack $ symbolVal (Proxy @s)

renamedTableOrigName :: forall t s t'. (t ~ RenamedTable s t', DatabaseTable t') => ByteString
renamedTableOrigName = tableName @t'

instance (DatabaseTable t, KnownSymbol s) => DatabaseTableLike (RenamedTable s t) where
  columnName (RenamedTable c) = columnName c
  columnTableName (RenamedTable _) = ColumnTableName { columnTableNameString = BS.pack $ symbolVal (Proxy @s), columnTableNameExplicit = True }

instance (DatabaseTable t, KnownSymbol s, InTable t a) => InTable (RenamedTable s t) a where
  columnRep = coerce (columnRep @t @a)