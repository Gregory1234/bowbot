{-# LANGUAGE GADTs #-}

module BowBot.DB.Typed.RenderQuery where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import BowBot.DB.Typed.DatabaseTable
import BowBot.DB.Typed.Types
import Data.List ((\\))

sqlQuote :: ByteString -> ByteString
sqlQuote x = "`" <> x <> "`"

renderColumnQualified :: DatabaseTableLike t => t a -> ByteString
renderColumnQualified c = let ColumnTableName tn _ = columnTableName c; cn = columnName c in sqlQuote tn <> "." <> sqlQuote cn

renderColumn :: DatabaseTableLike t => t a -> ByteString
renderColumn c = let ColumnTableName tn expl = columnTableName c; cn = columnName c in if expl then sqlQuote tn <> "." <> sqlQuote cn else sqlQuote cn

renderSomeCol :: DatabaseTableLike t => SomeCol t -> ByteString
renderSomeCol (SomeCol c) = renderColumn c

renderSelector :: DatabaseTableLike t => Selector t e a -> ByteString
renderSelector (EntSelector cols) = BS.intercalate "," . map renderSomeCol $ toSomeCols cols
renderSelector (ColSelector col) = renderColumn col
renderSelector ConstSelector = "?"
renderSelector (AndSelector2 s1 s2) = renderSelector s1 <> "," <> renderSelector s2

renderCondition :: DatabaseTableLike t => Condition t b -> ByteString
renderCondition NoCondition = ""
renderCondition (EntCondition cols) = BS.intercalate " AND " . map (\c -> renderSomeCol c <> " = ?") $ toSomeCols cols
renderCondition (FunCondition col fun) = renderSqlFunction fun (renderColumn col) <> " = ?"
renderCondition (InCondition col) = renderColumn col <> " IN ?"
renderCondition (ColCondition col) = renderColumn col <> " = ?"
renderCondition (AndCondition2 (renderCondition -> c1) (renderCondition -> c2)) = if BS.null c1 then c2 else c1 <> " AND " <> c2

renderWhereClause :: DatabaseTableLike t => Condition t b -> ByteString
renderWhereClause (renderCondition -> cond) = if BS.null cond then "" else " WHERE " <> cond

renderUpdateValues :: forall t a. DatabaseTable t => ColRep t a -> ByteString
renderUpdateValues cols = BS.intercalate "," . map (\(sqlQuote -> c) -> c <> " = VALUES(" <> c <> ")") $ map columnName' (toSomeCols cols) \\ map columnName' (someCols @t @(PrimaryKey t))

renderJoinOn :: (DatabaseTableLike t, DatabaseTableLike s) => ColRep t a -> ColRep s a -> ByteString
renderJoinOn colsT colsS = BS.intercalate "," $ zipWith (\(SomeCol c) (SomeCol d) -> renderColumnQualified c <> " = " <> renderColumnQualified d) (toSomeCols colsT) (toSomeCols colsS)

renderRenameSpec :: forall t. RenameSpec t -> ByteString
renderRenameSpec NoRename = sqlQuote (tableName @t)
renderRenameSpec Rename = sqlQuote (renamedTableOrigName @t) <> " AS " <> sqlQuote (renamedTableAlias @t)

renderJoinSpec :: forall t n. JoinSpec n t -> ByteString
renderJoinSpec (JoinLast t) = renderRenameSpec t
renderJoinSpec (JoinCons at as t s) = renderJoinSpec t <> " JOIN " <> renderRenameSpec s <> " ON " <> renderJoinOn at as

renderQuestionMarks :: ColRep t a -> ByteString
renderQuestionMarks cols = BS.intercalate "," $ map (const "?") (toSomeCols cols)