{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RoleAnnotations #-}

module BowBot.DB.Typed.Types where

import BowBot.DB.Typed.DatabaseTable
import GHC.TypeLits (KnownSymbol)
import Database.MySQL.Simple (Query)
import Data.Kind (Type)
import Data.ByteString (ByteString)
import Database.MySQL.Simple.Types (In)

type family SimpTup2 a b where
  SimpTup2 () b = b
  SimpTup2 a () = a
  SimpTup2 a b = (a,b)

newtype RightJ a = RightJ { fromRightJ :: a }

instance (DatabaseTableLike t, InTable s a) => InTable (JoinTable t s) (RightJ a) where
  columnRep = ColRep $ map (\(SomeCol c) -> SomeCol (JoinTRight c)) $ toSomeCols (columnRep @s @a)

newtype LeftJ a = LeftJ { fromLeftJ :: a }

instance (InTable t a, DatabaseTableLike s) => InTable (JoinTable t s) (LeftJ a) where
  columnRep = ColRep $ map (\(SomeCol c) -> SomeCol (JoinTLeft c)) $ toSomeCols (columnRep @t @a)

data Nat = NatZ | NatS Nat

type family SimpJ (n :: Nat) a where
  SimpJ 'NatZ a = a
  SimpJ ('NatS n) (RightJ a) = SimpJ n a
  SimpJ ('NatS n) (LeftJ a) = a
  SimpJ n (a, b) = (SimpJ n a, SimpJ n b)
  SimpJ _ a = a

newtype SqlFunction a i o = SqlFunction { renderSqlFunction :: ByteString -> ByteString }

data Selector t b a where
  EntSelector :: forall a t. ColRep t a -> Selector t () a
  ColSelector :: forall a t. t a -> Selector t () a
  ConstSelector :: forall a t. Selector t a a
  AndSelector2 :: Selector t b1 a1 -> Selector t b2 a2 -> Selector t (SimpTup2 b1 b2) (SimpTup2 a1 a2)

data Condition t b where
  EntCondition :: forall a t. ColRep t a -> Condition t a
  ColCondition :: forall a t. t a -> Condition t a
  FunCondition :: forall a i o t. t a -> SqlFunction a i o -> Condition t (SimpTup2 i o)
  InCondition :: forall a t. t a -> Condition t (In [a])
  NoCondition :: Condition t ()
  AndCondition2 :: Condition t b1 -> Condition t b2 -> Condition t (SimpTup2 b1 b2)

data JoinSpec n t where
  JoinLast :: forall t. DatabaseTableLike t => RenameSpec t -> JoinSpec 'NatZ t
  JoinCons :: forall a t s n. (DatabaseTableLike t, DatabaseTableLike s) => ColRep t a -> ColRep s a -> JoinSpec n t -> RenameSpec s -> JoinSpec ('NatS n) (JoinTable t s)

data RenameSpec t where
  NoRename :: forall t. DatabaseTable t => RenameSpec t
  Rename :: forall s t. (KnownSymbol s, DatabaseTable t) => RenameSpec (RenamedTable s t)


type TypedQuery :: Type -> Type -> Type
newtype TypedQuery i o = TypedQuery { fromTypedQuery :: Query } deriving Show
type role TypedQuery nominal nominal

type TypedCommand :: Type -> Type
newtype TypedCommand i = TypedCommand { fromTypedCommand :: Query } deriving Show
type role TypedCommand nominal