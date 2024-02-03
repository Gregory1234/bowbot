module Language.MySQL.Parser where

import Language.MySQL.ParserUtils
import Language.MySQL.AST
import Text.Parsec
import Text.Parsec.String
import Data.Char
import Data.Functor (($>))

parensParser :: Parser a -> Parser a
parensParser = withSpace . inParens

reservedParser :: String -> Parser String
reservedParser = withSpace . reserved

funNameParser :: Parser FunName
funNameParser = withSpace $ flip label "function name" $ FunName <$> many1 (satisfy (\x -> isUpper x || x == '_'))

typeNameGenParser :: Parser TypeName
typeNameGenParser = withSpace $ flip label "type name" $ TypeName <$> ((:) <$> satisfy isUpper <*> many (satisfy (\x -> isAlphaNum x || x == '_')))

typeNameNotFunParser :: Parser TypeName
typeNameNotFunParser = withSpace $ flip label "type name" $ try $ do
  n <- (:) <$> satisfy isUpper <*> many (satisfy (\x -> isAlphaNum x || x == '_'))
  if all (\x -> isUpper x || x == '_') n
    then unexpected n
    else return $ TypeName n

typeNameParser :: Parser TypeName
typeNameParser = typeNameNotFunParser <|> try (parensParser typeNameGenParser)

varNameParser :: Parser VarName
varNameParser = withSpace $ flip label "variable name" $ fmap VarName $ (:) <$> satisfy (\x -> isLower x || x == '_') <*> many (satisfy (\x -> isAlphaNum x || x == '_'))

typeParser :: Parser ParsedType
typeParser = (ParsedType . (:[]) <$> typeNameParser) <|> parensParser (ParsedType <$> many1 typeNameGenParser)

typeGenParser :: Parser ParsedType
typeGenParser = ParsedType <$> many1 typeNameGenParser

baseExpressionParser :: Parser Expression
baseExpressionParser = StringExpr <$> withSpace (stringLit '"') <*> optionMaybe typeParser
  <|> StringExpr <$> withSpace (stringLit '\'') <*> optionMaybe typeParser
  <|> IntExpr <$> withSpace numberLit <*> optionMaybe typeParser
  <|> ColExpr <$> fullColumnNameParser
  <|> VarExpr <$> varNameParser <*> optionMaybe typeParser
  <|> NullExpr <$> (reservedParser "NULL" *> optionMaybe typeParser)
  <|> FunExpr <$> funNameParser <*> parensParser (sepBy expressionParser commaParser)

listExpressionParser :: Parser ListExpression
listExpressionParser = VarListExpr <$> varNameParser
  <|> ListExpr <$> parensParser (sepBy expressionParser commaParser)

overrideExprParser :: Parser Expression
overrideExprParser = do
  e1 <- baseExpressionParser
  option e1 (OverrideExpr e1 <$> (reservedParser "OVERRIDE" *> typeParser))

eqOpParser :: Parser EqOp
eqOpParser = EqOp <$ reservedParser "="
  <|> NeqOp <$ reservedParser "<>"

compOpParser :: Parser CompOp
compOpParser = LeqOp <$ reservedParser "<="
  <|> GeqOp <$ reservedParser ">="
  <|> LtOp <$ reservedParser "<"
  <|> GtOp <$ reservedParser ">"

boolCompExprParser :: Parser Expression
boolCompExprParser = do
  e1 <- overrideExprParser
  option e1 (EqExpr e1 <$> eqOpParser <*> overrideExprParser
    <|> CompExpr e1 <$> compOpParser <*> overrideExprParser
    <|> InExpr e1 <$> (reservedParser "IN" *> listExpressionParser)
    <|> NotInExpr e1 <$> (reservedParser "NOT" *> reservedParser "IN" *> listExpressionParser))

expressionParser :: Parser Expression
expressionParser = boolCompExprParser `chainl1` (reservedParser "AND" $> AndExpr)

complexExpressionParser :: Parser ComplexExpression
complexExpressionParser = (typeNameParser >>= (\t -> option (ImplicitComplexExpr t) $ ComplexExpr (Just t) <$> parensParser complexExpressionListParser))
  <|> ComplexExpr Nothing <$> parensParser complexExpressionListParser
  <|> SimpleExpr <$> expressionParser

complexExpressionListParser :: Parser [ComplexExpression]
complexExpressionListParser = sepBy1 complexExpressionParser commaParser

tableNameParser :: Parser TableName
tableNameParser = TableName <$> withSpace backQuoteNameLit

columnNameParser :: Parser ColumnName
columnNameParser = ColumnName <$> withSpace backQuoteNameLit

fullColumnNameParser :: Parser FullColumnName
fullColumnNameParser = withSpace $ do
  n1 <- backQuoteNameLit
  option (FullColumnName Nothing (ColumnName n1)) $ FullColumnName (Just (TableName n1)) . ColumnName <$> (string "." *> backQuoteNameLit)

aliasedTableParser :: Parser AliasedTable
aliasedTableParser = AliasedTable <$> tableNameParser <*> optionMaybe (reservedParser "AS" *> tableNameParser)

tableJoinOnParser :: Parser TableJoinOn
tableJoinOnParser = TableJoinOn <$> columnNameParser <*> (reservedParser "=" *> fullColumnNameParser)

joinParser :: Parser (AliasedTable, TableJoinOn)
joinParser = (,) <$> (reservedParser "JOIN" *> aliasedTableParser) <*> (reservedParser "ON" *> tableJoinOnParser)

joinTablesParser :: Parser JoinTables
joinTablesParser = JoinTables <$> aliasedTableParser <*> many joinParser

whereClauseParser :: Parser WhereClause
whereClauseParser = WhereClause <$> optionMaybe (reservedParser "WHERE" *> expressionParser)

selectQueryParser :: Parser SelectQuery
selectQueryParser = SelectQuery <$> (reservedParser "SELECT" *> (implicitTupleExpr <$> complexExpressionListParser)) <*> (reservedParser "FROM" *> joinTablesParser) <*> whereClauseParser

insertTargetParser :: Parser InsertTarget
insertTargetParser = ColTarget <$> option DontUpdate (reservedParser "^" $> DoUpdate) <*> columnNameParser
  <|> (typeNameParser >>= (\t -> ComplexTarget (Just t) <$> parensParser insertTargetListParser <|> pure (ImplicitComplexTarget t)))
  <|> ComplexTarget Nothing <$> parensParser insertTargetListParser

insertTargetListParser :: Parser [InsertTarget]
insertTargetListParser = sepBy1 insertTargetParser commaParser

valuesRowParser :: Parser ValuesRow
valuesRowParser = ValuesRowVar <$> varNameParser <*> option ValuesRowSingle (reservedParser ".." $> ValuesRowMany)
  <|> ValuesRowExpr <$> parensParser complexExpressionListParser

insertSourceParser :: Parser InsertSource
insertSourceParser = ValuesSource <$> (reservedParser "VALUES" *> sepBy1 valuesRowParser commaParser)
  <|> SelectSource <$> selectQueryParser

simpleInsertSourceParser :: Parser SimpleInsertSource
simpleInsertSourceParser = SimpleValuesSource <$> (reservedParser "VALUES" *> parensParser complexExpressionListParser)

insertQueryParser :: Parser InsertQuery
insertQueryParser = reservedParser "INSERT" >> (InsertQuery <$> (reservedParser "INTO" *> tableNameParser) <*> (implicitTupleTarget <$> parensParser insertTargetListParser) <*> insertSourceParser
  <|> InsertAIQuery <$> (reservedParser "AI" *> optionMaybe typeParser) <*> (reservedParser "INTO" *> tableNameParser) <*> (implicitTupleTarget <$> parensParser insertTargetListParser) <*> simpleInsertSourceParser)

columnUpdateParser :: Parser ColumnUpdate
columnUpdateParser = ColumnUpdate <$> fullColumnNameParser <*> (reservedParser "=" *> expressionParser)

updateQueryParser :: Parser UpdateQuery
updateQueryParser = UpdateQuery <$> (reservedParser "UPDATE" *> joinTablesParser) <*> (reservedParser "SET" *> sepBy1 columnUpdateParser commaParser) <*> whereClauseParser

deleteQueryParser :: Parser DeleteQuery
deleteQueryParser = DeleteQuery <$> (reservedParser "DELETE" *> reservedParser "FROM" *> tableNameParser) <*> whereClauseParser

anyQueryParser :: Parser AnyQuery
anyQueryParser = SelectAnyQuery <$> selectQueryParser
  <|> InsertAnyQuery <$> insertQueryParser
  <|> UpdateAnyQuery <$> updateQueryParser
  <|> DeleteAnyQuery <$> deleteQueryParser