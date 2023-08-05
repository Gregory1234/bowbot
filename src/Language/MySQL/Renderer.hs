{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.MySQL.Renderer where

import Language.MySQL.AST
import Language.MySQL.TypedAST
import Language.Haskell.TH
import Control.Monad.Writer.Strict
import Data.Maybe
import qualified Data.Map as M
import Control.Monad.State.Strict
import Language.MySQL.Typechecker
import qualified Data.ByteString as BS
import Data.List ( intersperse )
import Language.MySQL.Query

pattern AppendE :: Exp -> Exp -> Exp
pattern AppendE x y <- (AppE (AppE (VarE ((== '(<>)) -> True)) x) y)
  where
    AppendE x y = AppE (AppE (VarE '(<>)) x) y

pattern StringE :: String -> Exp
pattern StringE s = LitE (StringL s)

combineString :: Exp -> Exp -> Exp
combineString (StringE a) (StringE b) = StringE (a ++ b)
combineString (StringE a) (AppendE (StringE b) ys) = AppendE (StringE (a ++ b)) ys
combineString (StringE a) ys = AppendE (StringE a) ys
combineString (AppendE a b) ys = combineString a (combineString b ys)
combineString xs ys = AppendE xs ys

data TypeOfVar = SingleVar | ListVar

newtype Renderer a = Renderer { runRenderer :: StateT (M.Map Name (Name, TypeOfVar)) Q a }
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadIO, MonadState (M.Map Name (Name, TypeOfVar)))

instance MonadQ Renderer where
  liftQ = Renderer . lift

type StrRenderer = WriterT [Exp] Renderer

runStrRenderer :: StrRenderer () -> Renderer Exp
runStrRenderer r = foldr1 combineString <$> execWriterT r

reifyRender :: StrRenderer () -> StrRenderer Exp
reifyRender = fmap (foldr1 combineString . snd) . censor (const []) . listen

instance MonadQ StrRenderer where
  liftQ = lift . liftQ

instance MonadQ (StateT (Either Bool Exp) StrRenderer) where
  liftQ = lift . liftQ

emit :: String -> StrRenderer ()
emit s = tell [StringE s]

emitQ :: Q Exp -> StrRenderer ()
emitQ q = do
  e <- liftQ q
  tell [e]

joinSep :: String -> [StrRenderer ()] -> StrRenderer ()
joinSep sep = sequence_ . intersperse (emit sep)

listRenderer :: [StrRenderer ()] -> StrRenderer ()
listRenderer = joinSep ","

withSpaces :: [StrRenderer ()] -> StrRenderer ()
withSpaces = joinSep " "

parensRenderer :: StrRenderer a -> StrRenderer a
parensRenderer a = emit "(" *> a <* emit ")"

addVar :: (MonadState (M.Map Name (Name, TypeOfVar)) m, MonadQ m) => Name -> TypeOfVar -> m Name
addVar n t = do
  s <- get
  case s M.!? n of
    Just (n', _) -> return n'
    Nothing -> do
      n' <- liftQ $ newName (nameBase n ++ "Str")
      modify (M.insert n (n', t))
      return n'
  

expressionRenderer :: TypedExpression -> StrRenderer ()
expressionRenderer (TypedStringExpr e _) = emit e
expressionRenderer (TypedIntExpr e _) = emit e
expressionRenderer (TypedVarExpr n _) = do
  n' <- addVar n SingleVar
  tell [VarE n']
expressionRenderer (TypedColExpr n _) = emit $ ppFullColumnName n
expressionRenderer (TypedAndExpr e1 e2) = expressionRenderer e1 *> emit " AND " *> expressionRenderer e2
expressionRenderer (TypedEqExpr e1 e2) = expressionRenderer e1 *> emit "=" *> expressionRenderer e2
expressionRenderer (TypedGtExpr e1 e2) = expressionRenderer e1 *> emit ">" *> expressionRenderer e2
expressionRenderer (TypedFunExpr (TypedFunction (FunName n) _ _) s) = emit n *> parensRenderer (listRenderer (map expressionRenderer s))
expressionRenderer (TypedInExpr e1 e2) = listExpressionInRenderer True e2 (expressionRenderer e1)
expressionRenderer (TypedNotInExpr e1 e2) = listExpressionInRenderer False e2 (expressionRenderer e1)
expressionRenderer (TypedIsNullExpr e) = expressionRenderer e *> emit " IS NULL"
expressionRenderer (TypedIsNotNullExpr e) = expressionRenderer e *> emit " IS NOT NULL"
expressionRenderer (TypedOverrideExpr e _) = expressionRenderer e
expressionRenderer (TypedNullExpr _) = emit "NULL"

listExpressionInRenderer :: Bool -> TypedListExpression -> StrRenderer () -> StrRenderer ()
listExpressionInRenderer True (TypedVarListExpr n) e = do
  n' <- addVar n ListVar
  e' <- reifyRender e
  emitQ [| if null $(varE n') then "0" else $(pure e') <> " IN (" <> BS.intercalate "," $(varE n') <> ")" |]
listExpressionInRenderer True (TypedListExpr []) _ = emit "0"
listExpressionInRenderer True (TypedListExpr s) e = e *> emit " IN " *> parensRenderer (listRenderer (map expressionRenderer s))
listExpressionInRenderer False (TypedVarListExpr n) e = do
  n' <- addVar n ListVar
  e' <- reifyRender e
  emitQ [| if null $(varE n') then "1" else $(pure e') <> " NOT IN (" <> BS.intercalate "," $(varE n') <> ")" |]
listExpressionInRenderer False (TypedListExpr []) _ = emit "1"
listExpressionInRenderer False (TypedListExpr s) e = e *> emit " NOT IN " *> parensRenderer (listRenderer (map expressionRenderer s))


complexExpressionRenderer :: TypedComplexExpression -> StrRenderer ()
complexExpressionRenderer (TypedComplexExpr _ s) = listRenderer $ map complexExpressionRenderer s
complexExpressionRenderer (TypedSimpleExpr e) = expressionRenderer e

aliasedTableRenderer :: AliasedTable -> StrRenderer ()
aliasedTableRenderer (AliasedTable t Nothing) = emit $ ppTableName t
aliasedTableRenderer (AliasedTable t (Just a)) = emit $ ppTableName t ++ " AS " ++ ppTableName a

tableJoinRenderer :: (AliasedTable, TableJoinOn) -> StrRenderer ()
tableJoinRenderer (t@(AliasedTable tn ta), TableJoinOn c c') = 
  aliasedTableRenderer t *> emit (" ON " ++ ppTableName (fromMaybe tn ta) ++ "." ++ ppColumnName c ++ "=" ++ ppFullColumnName c')

joinTablesRenderer :: JoinTables -> StrRenderer ()
joinTablesRenderer (JoinTables t ts) = joinSep " JOIN " $ aliasedTableRenderer t : map tableJoinRenderer ts

whereClauseRenderer :: TypedWhereClause -> StrRenderer ()
whereClauseRenderer (TypedWhereClause Nothing) = emit ""
whereClauseRenderer (TypedWhereClause (Just e)) = emit "WHERE " *> expressionRenderer e

selectQueryStringRenderer :: TypedSelectQuery -> StrRenderer ()
selectQueryStringRenderer (TypedSelectQuery s t w) = withSpaces [emit "SELECT", complexExpressionRenderer s, emit "FROM", joinTablesRenderer t, whereClauseRenderer w]

renderTypecheckConstraint :: TypecheckConstraint -> Q Exp -> Q Exp
renderTypecheckConstraint (NameHasType n t) e = [| reqEqType' @($(pure t)) $(varE n) $e |]
renderTypecheckConstraint (NameTypesEq n1 n2) e = [| reqEqType $(varE n1) $(varE n2) $e |]
renderTypecheckConstraint (TypeIsString t) e = [| reqMysqlString' @($(pure t)) $e |]
renderTypecheckConstraint (TypeIsInt t) e = [| reqMysqlInt' @($(pure t)) $e |]
renderTypecheckConstraint (VarIsString n) e = [| reqMysqlString $(varE n) $e |]
renderTypecheckConstraint (VarIsInt n) e = [| reqMysqlInt $(varE n) $e |]
renderTypecheckConstraint (VarIsTuple n ns) e = [| let $(tupP (map varP ns)) = $(varE n) in $e |]
renderTypecheckConstraint (VarIsList n n') e = [| let $(varP n') = head $(varE n) in $e |]

renderTypeInhabitant :: PartialType -> Q Exp
renderTypeInhabitant (TupleType [t]) = renderTypeInhabitant t
renderTypeInhabitant (TupleType s) = tupE (map renderTypeInhabitant s)
renderTypeInhabitant (RealType t) = [| undefined :: $(pure t) |]
renderTypeInhabitant (VarType n) = varE n

renderVar :: Name -> Name -> Name -> TypeOfVar -> Q Stmt
renderVar conn origName strName SingleVar = bindS (varP strName) [| toMysql $(varE conn) $(varE origName) |]
renderVar conn origName strName ListVar = bindS (varP strName) [| mapM (toMysql $(varE conn)) $(varE origName) |]

renderQuery :: [TypecheckConstraint] -> M.Map Name (Name, TypeOfVar) -> Q Exp -> Q Exp
renderQuery tc vars q = do
  conn <- newName "conn"
  lam1E (if null vars then wildP else varP conn) $ doE $ map (\(origName, (strName, varType)) -> renderVar conn origName strName varType) (M.toList vars)
    ++ [noBindS $ foldr renderTypecheckConstraint [| return $q |] tc]

renderSelectQuery :: TypedSelectQuery -> [TypecheckConstraint] -> Q Exp
renderSelectQuery q@(TypedSelectQuery s _ _) tc = do
  (str, vars) <- runStateT (runRenderer $ runStrRenderer $ selectQueryStringRenderer q) M.empty
  renderQuery tc vars [| mkQuery $(renderTypeInhabitant (typedComplexExprType s)) $(pure str) |]

insertTargetRenderer :: TypedInsertTarget -> StrRenderer ()
insertTargetRenderer (TypedColTarget c _) = emit $ ppColumnName c
insertTargetRenderer (TypedComplexTarget _ s) = listRenderer (map insertTargetRenderer s)

updateOnDuplicateListRenderer :: UpdateOnDuplicateList -> StrRenderer ()
updateOnDuplicateListRenderer (UpdateOnDuplicateList []) = emit ""
updateOnDuplicateListRenderer (UpdateOnDuplicateList xs) = emit "ON DUPLICATE KEY UPDATE " *> listRenderer (map helper xs)
  where
    helper n = emit $ ppColumnName n ++ "=VALUES(" ++ ppColumnName n ++ ")"

firstCommaRenderer :: StateT (Either Bool Exp) StrRenderer ()
firstCommaRenderer = do
  s <- get
  lift $ case s of
    Left True -> emit ""
    Left False -> emit ","
    Right isFirst -> emitQ [| if $(pure isFirst) then "" else "," |]

-- TODO: simplify this
valuesRowRender :: TypedValuesRow -> StateT (Either Bool Exp) StrRenderer ()
valuesRowRender (TypedValuesRowExpr e) = firstCommaRenderer *> lift (parensRenderer $ complexExpressionRenderer e) *> put (Left False)
valuesRowRender (TypedValuesRowVar n ValuesRowSingle) = do
  n' <- lift $ addVar n SingleVar
  firstCommaRenderer
  put (Left False)
  lift $ parensRenderer $ emitQ (varE n')
valuesRowRender (TypedValuesRowVar n ValuesRowMany) = do
  n' <- lift $ addVar n ListVar
  s <- get
  case s of
    Left True -> lift (emitQ [| BS.intercalate "," (map (\x -> "(" <> x <> ")") $(varE n')) |]) *> (put . Right =<< liftQ [| null $(varE n)|])
    Left False -> lift (emitQ [| mconcat (map (\x -> ",(" <> x <> ")") $(varE n')) |])
    Right isFirst -> lift (emitQ [| (if $(pure isFirst) then "" else ",") |] *> emitQ [| BS.intercalate "," (map (\x -> "(" <> x <> ")") $(varE n')) |])
      *> (put . Right =<< lift (liftQ [| $(pure isFirst) && null $(varE n)|]))

insertSourceRenderer :: TypedInsertSource -> StrRenderer ()
insertSourceRenderer (TypedValuesSource rows) = emit "VALUES" *> evalStateT (mapM_ valuesRowRender rows) (Left True)
insertSourceRenderer (TypedSelectSource q) = selectQueryStringRenderer q

insertQueryStringRenderer :: TypedInsertQuery -> StrRenderer ()
insertQueryStringRenderer (TypedInsertQuery tn t s u) = withSpaces [emit "INSERT INTO", emit $ ppTableName tn, parensRenderer $ insertTargetRenderer t, insertSourceRenderer s, updateOnDuplicateListRenderer u]

insertSourceCheckRenderer :: TypedInsertSource -> Q Exp -> Q Exp
insertSourceCheckRenderer (TypedValuesSource (mapM (\case (TypedValuesRowVar n ValuesRowMany) -> Just n; _ -> Nothing) -> Just lists)) e 
  = [| if $(foldl1 (\a b -> [| $a && $b |]) (map (\n -> [| null $(varE n) |]) lists)) then \_ -> return (Command Nothing) else $e |]
insertSourceCheckRenderer _ e = e

renderInsertQuery :: TypedInsertQuery -> [TypecheckConstraint] -> Q Exp
renderInsertQuery q@(TypedInsertQuery _ _ s _) tc = do
  (str, vars) <- runStateT (runRenderer $ runStrRenderer $ insertQueryStringRenderer q) M.empty
  insertSourceCheckRenderer s $ renderQuery tc vars [| Command (Just $(pure str)) |]

columnUpdateRenderer :: TypedColumnUpdate -> StrRenderer ()
columnUpdateRenderer (TypedColumnUpdate c e) = emit (ppFullColumnName c ++ "=") *> expressionRenderer e

updateQueryStringRenderer :: TypedUpdateQuery -> StrRenderer ()
updateQueryStringRenderer (TypedUpdateQuery t u w) = withSpaces [emit "UPDATE", joinTablesRenderer t, emit "SET", listRenderer $ map columnUpdateRenderer u, whereClauseRenderer w]

renderUpdateQuery :: TypedUpdateQuery -> [TypecheckConstraint] -> Q Exp
renderUpdateQuery q tc = do
  (str, vars) <- runStateT (runRenderer $ runStrRenderer $ updateQueryStringRenderer q) M.empty
  renderQuery tc vars [| Command (Just $(pure str)) |]

deleteQueryStringRenderer :: TypedDeleteQuery -> StrRenderer ()
deleteQueryStringRenderer (TypedDeleteQuery t w) = withSpaces [emit $ "DELETE FROM " ++ ppTableName t, whereClauseRenderer w]

renderDeleteQuery :: TypedDeleteQuery -> [TypecheckConstraint] -> Q Exp
renderDeleteQuery q tc = do
  (str, vars) <- runStateT (runRenderer $ runStrRenderer $ deleteQueryStringRenderer q) M.empty
  renderQuery tc vars [| Command (Just $(pure str)) |]

renderAnyQuery :: TypedAnyQuery -> [TypecheckConstraint] -> Q Exp
renderAnyQuery (TypedSelectAnyQuery q) = renderSelectQuery q
renderAnyQuery (TypedInsertAnyQuery q) = renderInsertQuery q
renderAnyQuery (TypedUpdateAnyQuery q) = renderUpdateQuery q
renderAnyQuery (TypedDeleteAnyQuery q) = renderDeleteQuery q