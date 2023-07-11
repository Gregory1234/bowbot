{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

module BowBot.DB.Typed.Query(
  module BowBot.DB.Typed.Query, Selector(..), Condition(..), SqlFunction(..), JoinSpec(..), RenameSpec(..), TypedQuery, TypedCommand
) where
  
import BowBot.DB.Typed.DatabaseTable
import BowBot.DB.Typed.Types
import Database.MySQL.Simple.Types (Query(..))
import BowBot.DB.Typed.RenderQuery
import Data.Type.Equality ((:~:)(..))


selectGenQuery :: forall t a b e n. DatabaseTableLike t => Selector t e a -> JoinSpec n t -> Condition t b -> TypedQuery (SimpJ n (SimpTup2 e b)) (SimpJ n a)
selectGenQuery selector joins condition = TypedQuery $ Query $ "SELECT " <> renderSelector selector <> " FROM " <> renderJoinSpec joins <> renderWhereClause condition

selectSingleGenQuery :: forall t a b e. DatabaseTable t => Selector t e a -> Condition t b -> TypedQuery (SimpTup2 e b) a
selectSingleGenQuery selector = selectGenQuery selector (JoinLast NoRename)

selectConditionQuery :: forall t a b. (DatabaseTable t, InTable t a) => Condition t b -> TypedQuery b a
selectConditionQuery = selectSingleGenQuery @t (EntSelector columnRep)

selectAllQuery :: forall t a. (DatabaseTable t, InTable t a) => TypedQuery () a
selectAllQuery = selectSingleGenQuery @t (EntSelector columnRep) NoCondition

selectAllQuery' :: forall a. (DatabaseTable (MainTable a), InTable (MainTable a) a) => TypedQuery () a
selectAllQuery' = selectAllQuery @(MainTable a)

selectByQuery :: forall t a b. (DatabaseTable t, InTable t a, InTable t b) => TypedQuery b a
selectByQuery = selectSingleGenQuery @t (EntSelector columnRep) (EntCondition columnRep)

selectByPrimaryQuery :: forall t a. (DatabaseTable t, InTable t a) => TypedQuery (PrimaryKey t) a
selectByPrimaryQuery = selectByQuery @t

selectByColQuery :: forall t a b. (DatabaseTable t, InTable t a) => t b -> TypedQuery b a
selectByColQuery col = selectSingleGenQuery @t (EntSelector columnRep) (ColCondition col)

valuesQuery :: forall a t. ColRep t a -> TypedQuery a a
valuesQuery cols = TypedQuery $ Query $ "VALUES (" <> renderQuestionMarks cols <> ")"

selectJoinGenQuery :: forall t s a b e. (DatabaseTable t, DatabaseTable s, InTable t (PrimaryKey s)) => Selector (JoinTable t s) e a -> Condition (JoinTable t s) b -> TypedQuery (SimpJ ('NatS 'NatZ) (SimpTup2 e b)) (SimpJ ('NatS 'NatZ) a)
selectJoinGenQuery selector = selectGenQuery selector (JoinCons (columnRep @t @(PrimaryKey s)) (columnRep @s @(PrimaryKey s)) (JoinLast NoRename) NoRename)

selectJoinByLeftQuery :: forall t s a b. (DatabaseTable t, DatabaseTable s, InTable t (PrimaryKey s), InTable t a, InTable s b) => TypedQuery a b
selectJoinByLeftQuery = selectJoinGenQuery @t @s (EntSelector @(RightJ b) columnRep) (EntCondition @(LeftJ a) columnRep)

selectJoinByRightQuery :: forall t s a b. (DatabaseTable t, DatabaseTable s, InTable t (PrimaryKey s), InTable t a, InTable s b) => TypedQuery b a
selectJoinByRightQuery = selectJoinGenQuery @t @s (EntSelector @(LeftJ a) columnRep) (EntCondition @(RightJ b) columnRep)


insertUpdateGenQuery :: forall t a b c. DatabaseTable t => ColRep t a -> TypedQuery b a -> Either (c :~: ()) (TypedCommand c) -> TypedCommand (SimpTup2 c b)
insertUpdateGenQuery cols q (Left Refl) = TypedCommand $ Query $ "INSERT INTO " <> sqlQuote (tableName @t) <> "(" <> renderSelector (EntSelector cols) <> ") " <> fromQuery (fromTypedQuery q)
insertUpdateGenQuery cols q (Right d) = TypedCommand $ Query $ "INSERT INTO " <> sqlQuote (tableName @t) <> "(" <> renderSelector (EntSelector cols) <> ") " <> fromQuery (fromTypedQuery q) <> " ON DUPLICATE KEY " <> fromQuery (fromTypedCommand d)

updateValuesGenQuery :: forall t a. DatabaseTable t => ColRep t a -> TypedCommand ()
updateValuesGenQuery cols = TypedCommand $ Query $ "UPDATE " <> renderUpdateValues cols

noUpdateValues :: Either (() :~: ()) (TypedCommand ())
noUpdateValues = Left Refl

insertGenQuery :: forall t a b. (DatabaseTable t) => ColRep t a -> TypedQuery b a -> TypedCommand b
insertGenQuery cols q = insertUpdateGenQuery cols q (Right (updateValuesGenQuery cols))

insertNoUpdateQuery :: forall t a. (DatabaseTable t, InTable t a) => TypedCommand a
insertNoUpdateQuery = insertUpdateGenQuery (columnRep @t @a) (valuesQuery (columnRep @t @a)) noUpdateValues

insertQuery :: forall t a. (DatabaseTable t, InTable t a) => TypedCommand a
insertQuery = insertGenQuery (columnRep @t @a) (valuesQuery (columnRep @t @a))

insertQuery' :: forall a. (DatabaseTable (MainTable a), InTable (MainTable a) a) => TypedCommand a
insertQuery' = insertQuery @(MainTable a)