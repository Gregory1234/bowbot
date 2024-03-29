{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.MySQL.TypedAST where

import Language.MySQL.AST
import Language.Haskell.TH (Name, Type (..))
import Data.Text (Text) 

data PartialType = RealType Type | VarType Name | TupleType [PartialType] deriving (Show, Eq)

implicitTupleType :: [PartialType] -> PartialType
implicitTupleType [t] = t
implicitTupleType ts = TupleType ts

textType :: PartialType
textType = RealType $ ConT ''Text
integerType :: PartialType
integerType = RealType $ ConT ''Integer
boolType :: PartialType
boolType = RealType $ ConT ''Bool

data TypedFunction = TypedFunction FunName [Type] Type deriving (Show, Eq)

data TypedExpression
  = TypedStringExpr String PartialType
  | TypedIntExpr String PartialType
  | TypedVarExpr Name PartialType
  | TypedColExpr FullColumnName Type
  | TypedAndExpr TypedExpression TypedExpression
  | TypedEqExpr TypedExpression EqOp TypedExpression
  | TypedCompExpr TypedExpression CompOp TypedExpression
  | TypedFunExpr TypedFunction [TypedExpression]
  | TypedInExpr TypedExpression TypedListExpression
  | TypedNotInExpr TypedExpression TypedListExpression
  | TypedOverrideExpr TypedExpression Type
  | TypedNullExpr PartialType
  deriving (Show, Eq)

typedExprType :: TypedExpression -> PartialType
typedExprType (TypedStringExpr _ t) = t
typedExprType (TypedIntExpr _ t) = t
typedExprType (TypedVarExpr _ t) = t
typedExprType (TypedColExpr _ t) = RealType t
typedExprType (TypedAndExpr _ _) = boolType
typedExprType (TypedEqExpr{}) = boolType
typedExprType (TypedCompExpr{}) = boolType
typedExprType (TypedFunExpr (TypedFunction _ _ t) _) = RealType t
typedExprType (TypedInExpr _ _) = boolType
typedExprType (TypedNotInExpr _ _) = boolType
typedExprType (TypedOverrideExpr _ t) = RealType t
typedExprType (TypedNullExpr t) = t

data TypedListExpression
  = TypedVarListExpr Name
  | TypedListExpr [TypedExpression]
  deriving (Show, Eq)

data TypedComplexExpression
  = TypedComplexExpr PartialType [TypedComplexExpression]
  | TypedSimpleExpr TypedExpression
  deriving (Show, Eq)

typedImplicitTuple :: [TypedComplexExpression] -> TypedComplexExpression
typedImplicitTuple [e] = e
typedImplicitTuple es = TypedComplexExpr (TupleType $ map typedComplexExprType es) es

typedComplexExprType :: TypedComplexExpression -> PartialType
typedComplexExprType (TypedComplexExpr t _) = t
typedComplexExprType (TypedSimpleExpr e) = typedExprType e

newtype TypedWhereClause = TypedWhereClause (Maybe TypedExpression) deriving (Show, Eq)

data TypedSelectQuery = TypedSelectQuery TypedComplexExpression JoinTables TypedWhereClause deriving (Show, Eq)

data TypedValuesRow = TypedValuesRowVar Name ValuesRowType | TypedValuesRowExpr TypedComplexExpression deriving (Show, Eq)

data TypedInsertTarget
  = TypedColTarget ColumnName Type
  | TypedComplexTarget PartialType [TypedInsertTarget]
  deriving (Show, Eq)

typedInsertTargetType :: TypedInsertTarget -> PartialType
typedInsertTargetType (TypedColTarget _ t) = RealType t
typedInsertTargetType (TypedComplexTarget t _) = t

data TypedInsertSource
  = TypedValuesSource [TypedValuesRow]
  | TypedSelectSource TypedSelectQuery
  deriving (Show, Eq)

data TypedSimpleInsertSource
  = TypedSimpleValuesSource TypedComplexExpression
  deriving (Show, Eq)

newtype UpdateOnDuplicateList = UpdateOnDuplicateList [ColumnName] deriving (Show, Eq)

data TypedInsertQuery
  = TypedInsertQuery TableName TypedInsertTarget TypedInsertSource UpdateOnDuplicateList
  | TypedInsertAIQuery Type TableName TypedInsertTarget TypedSimpleInsertSource UpdateOnDuplicateList
  deriving (Show, Eq)

data TypedColumnUpdate = TypedColumnUpdate FullColumnName TypedExpression deriving (Show, Eq)

data TypedUpdateQuery = TypedUpdateQuery JoinTables [TypedColumnUpdate] TypedWhereClause deriving (Show, Eq)

data TypedDeleteQuery = TypedDeleteQuery TableName TypedWhereClause deriving (Show, Eq)

data TypedAnyQuery
  = TypedSelectAnyQuery TypedSelectQuery
  | TypedInsertAnyQuery TypedInsertQuery
  | TypedUpdateAnyQuery TypedUpdateQuery
  | TypedDeleteAnyQuery TypedDeleteQuery
  deriving (Show, Eq)
  