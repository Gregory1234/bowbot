module Language.MySQL.AST where

newtype VarName = VarName String deriving (Show, Eq)
newtype ParsedType = ParsedType [TypeName] deriving (Show, Eq, Ord)
newtype TypeName = TypeName String deriving (Show, Eq, Ord)

newtype ColumnName = ColumnName String deriving (Show, Eq)
newtype TableName = TableName String deriving (Show, Eq)
newtype FunName = FunName String deriving (Show, Eq, Ord)

ppColumnName :: ColumnName -> String
ppColumnName (ColumnName c) = "`" ++ c ++ "`"
ppTableName :: TableName -> String
ppTableName (TableName c) = "`" ++ c ++ "`"

data FullColumnName = FullColumnName (Maybe TableName) ColumnName deriving (Show, Eq)

ppFullColumnName :: FullColumnName -> String
ppFullColumnName (FullColumnName Nothing c) = ppColumnName c
ppFullColumnName (FullColumnName (Just t) c) = ppTableName t ++ "." ++ ppColumnName c

data Expression
  = StringExpr String (Maybe ParsedType)
  | IntExpr String (Maybe ParsedType)
  | VarExpr VarName (Maybe ParsedType)
  | ColExpr FullColumnName
  | AndExpr Expression Expression
  | EqExpr Expression Expression
  | GtExpr Expression Expression
  | FunExpr FunName [Expression]
  | InExpr Expression ListExpression
  | NotInExpr Expression ListExpression -- TODO: allow normal comparisons with null (have to render IS)
  | IsNullExpr Expression
  | IsNotNullExpr Expression
  | OverrideExpr Expression ParsedType
  | NullExpr (Maybe ParsedType)
  deriving (Show, Eq)

data ListExpression
  = VarListExpr VarName
  | ListExpr [Expression]
  deriving (Show, Eq)

data ComplexExpression
  = ComplexExpr (Maybe TypeName) [ComplexExpression]
  | ImplicitComplexExpr TypeName
  | SimpleExpr Expression
  deriving (Show, Eq)

implicitTupleExpr :: [ComplexExpression] -> ComplexExpression
implicitTupleExpr [e] = e
implicitTupleExpr es = ComplexExpr Nothing es

data AliasedTable = AliasedTable TableName (Maybe TableName) deriving (Show, Eq)

data TableJoinOn = TableJoinOn ColumnName FullColumnName deriving (Show, Eq)

data JoinTables
  = JoinTables AliasedTable [(AliasedTable, TableJoinOn)]
  deriving (Show, Eq)

newtype WhereClause = WhereClause (Maybe Expression) deriving (Show, Eq)

data SelectQuery = SelectQuery ComplexExpression JoinTables WhereClause deriving (Show, Eq)

data UpdateOnDuplicateKey = DoUpdate | DontUpdate deriving (Show, Eq)

data InsertTarget
  = ColTarget UpdateOnDuplicateKey ColumnName
  | ComplexTarget (Maybe TypeName) [InsertTarget]
  | ImplicitComplexTarget TypeName
  deriving (Show, Eq)

implicitTupleTarget :: [InsertTarget] -> InsertTarget
implicitTupleTarget [e] = e
implicitTupleTarget es = ComplexTarget Nothing es

data ValuesRowType = ValuesRowMany | ValuesRowSingle deriving (Show, Eq)

data ValuesRow = ValuesRowVar VarName ValuesRowType | ValuesRowExpr [ComplexExpression] deriving (Show, Eq)

data InsertSource 
  = ValuesSource [ValuesRow]
  | SelectSource SelectQuery
  deriving (Show, Eq)

data InsertQuery = InsertQuery TableName InsertTarget InsertSource deriving (Show, Eq)

data ColumnUpdate = ColumnUpdate FullColumnName Expression deriving (Show, Eq)

data UpdateQuery = UpdateQuery JoinTables [ColumnUpdate] WhereClause deriving (Show, Eq)

data DeleteQuery = DeleteQuery TableName WhereClause deriving (Show, Eq)

data AnyQuery
  = SelectAnyQuery SelectQuery
  | InsertAnyQuery InsertQuery
  | UpdateAnyQuery UpdateQuery
  | DeleteAnyQuery DeleteQuery
  deriving (Show, Eq)