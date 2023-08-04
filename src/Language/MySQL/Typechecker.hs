module Language.MySQL.Typechecker where

import Language.MySQL.AST
import Language.MySQL.TypedAST
import Language.Haskell.TH
import Control.Monad.Writer.Strict
import Data.Maybe
import Control.Applicative
import Data.Foldable (for_, foldlM)
import Control.Monad.Reader
import qualified Data.Map as M

data TypecheckConstraint
  = NameHasType Name Type
  | NameTypesEq Name Name
  | TypeIsString Type
  | TypeIsInt Type
  | VarIsString Name
  | VarIsInt Name
  | VarIsTuple Name [Name]
  | VarIsList Name Name
  deriving (Show, Eq)

class (MonadIO m, MonadFail m) => MonadQ m where liftQ :: Q a -> m a
instance MonadQ EarlyTypecheck where liftQ = lift
instance MonadQ Typecheck where liftQ = lift . lift

newtype Schema = Schema [(TableName, ColumnName, Type)] deriving (Show, Eq)

newtype SqlFuns = SqlFuns (M.Map FunName ([ParsedType], ParsedType))

type EarlyTypecheck = WriterT [TypecheckConstraint] Q

type Typecheck = ReaderT (Schema, SqlFuns) EarlyTypecheck

partialTypeIsString :: (MonadWriter [TypecheckConstraint] m, MonadFail m) => PartialType -> m ()
partialTypeIsString (RealType t) = tell [TypeIsString t]
partialTypeIsString (VarType t) = tell [VarIsString t]
partialTypeIsString (TupleType _) = fail "Tuple type is never string!"

partialTypeIsInt :: (MonadWriter [TypecheckConstraint] m, MonadFail m) => PartialType -> m ()
partialTypeIsInt (RealType t) = tell [TypeIsInt t]
partialTypeIsInt (VarType t) = tell [VarIsInt t]
partialTypeIsInt (TupleType _) = fail "Tuple type is never int!"

couldntMatchTypes :: PartialType -> PartialType -> String
couldntMatchTypes t1 t2 = "Couldn't match types: " ++ show t1 ++ " and " ++ show t2 -- TODO: nicer errors?

partialTypeEqual :: (MonadWriter [TypecheckConstraint] m, MonadQ m) => PartialType -> PartialType -> m ()
partialTypeEqual (RealType t1) (RealType t2)
  | t1 == t2 = pure ()
  | otherwise = fail $ couldntMatchTypes (RealType t1) (RealType t2)
partialTypeEqual (RealType t1) (VarType t2) = tell [NameHasType t2 t1]
partialTypeEqual (VarType t1) (RealType t2) = tell [NameHasType t1 t2]
partialTypeEqual (VarType t1) (VarType t2) = tell [NameTypesEq t2 t1]
partialTypeEqual (TupleType t1) (TupleType t2)
  | length t1 /= length t2 = fail $ tupleLengthMismatch t1 t2
  | otherwise = zipWithM_ partialTypeEqual t1 t2
partialTypeEqual (RealType t1) (TupleType t2) = fail $ couldntMatchTypes (RealType t1) (TupleType t2)
partialTypeEqual (TupleType t1) (RealType t2) = fail $ couldntMatchTypes (TupleType t1) (RealType t2)
partialTypeEqual (VarType t1) (TupleType t2) = do
  ns <- liftQ $ mapM (const (newName (nameBase t1))) t2
  tell [VarIsTuple t1 ns]
  zipWithM_ (partialTypeEqual . VarType) ns t2
partialTypeEqual (TupleType t1) (VarType t2) = do
  ns <- liftQ $ mapM (const (newName (nameBase t2))) t1
  tell [VarIsTuple t2 ns]
  zipWithM_ (\t -> partialTypeEqual t . VarType) t1 ns

getColumnType :: FullColumnName -> Typecheck Type
getColumnType fcn@(FullColumnName t c) = do
  (Schema s, _) <- ask
  case filter (\(t',c',_) -> c == c' && (isNothing t || t == Just t')) s of
    [(_,_,typ)] -> return typ
    [] -> fail $ "Couldn't find column: " ++ ppFullColumnName fcn ++ "!"
    _ -> fail $ "Ambigous column: " ++ ppFullColumnName fcn ++ "!"

typecheckType :: MonadQ m => ParsedType -> m Type
typecheckType (ParsedType t) = do
  realTypes <- mapM (fmap ConT . typecheckTypeName) t
  return $ foldl1 AppT realTypes

typecheckTypeName :: MonadQ m => TypeName -> m Name
typecheckTypeName (TypeName n) = do
  n' <- liftQ $ lookupTypeName n
  case n' of
    Nothing -> fail $ "Type not found: " ++ n ++ "!"
    Just n'' -> return n''

typecheckVarName :: MonadQ m => VarName -> m Name
typecheckVarName (VarName n) = do
  n' <- liftQ $ lookupValueName n
  case n' of
    Nothing -> fail $ "Var not found: " ++ n ++ "!"
    Just n'' -> return n''

getTypedFunction :: FunName -> Typecheck TypedFunction
getTypedFunction n = do
  (_, SqlFuns s) <- ask
  case s M.!? n of
    Nothing -> fail $ "Function not found: " ++ show n ++ "!"
    Just (args, res) -> TypedFunction n <$> mapM typecheckType args <*> typecheckType res

typecheckListExpr :: ListExpression -> PartialType -> Typecheck TypedListExpression
typecheckListExpr (VarListExpr nUntyped) t = do
  n <- typecheckVarName nUntyped
  n' <- liftQ $ newName (nameBase n)
  tell [VarIsList n n']
  partialTypeEqual t (VarType n')
  return $ TypedVarListExpr n
typecheckListExpr (ListExpr []) _ = pure $ TypedListExpr []
typecheckListExpr (ListExpr (e1:es)) t = do
  t1 <- typecheckExpr e1 (Just t)
  ts <- mapM (`typecheckExpr` Just t) es
  return $ TypedListExpr (t1:ts)
  

typecheckExpr :: Expression -> Maybe PartialType -> Typecheck TypedExpression
typecheckExpr (StringExpr s t1Untyped) t2 = do
  t1 <- mapM typecheckType t1Untyped
  let t = RealType <$> t1 <|> t2
  for_ t1 $ \t1' -> for_ t2 $ \t2' -> partialTypeEqual (RealType t1') t2'
  for_ t partialTypeIsString
  return $ TypedStringExpr s $ fromMaybe textType t
typecheckExpr (IntExpr s t1Untyped) t2 = do
  t1 <- mapM typecheckType t1Untyped
  let t = RealType <$> t1 <|> t2
  for_ t1 $ \t1' -> for_ t2 $ \t2' -> partialTypeEqual (RealType t1') t2'
  for_ t partialTypeIsInt
  return $ TypedIntExpr s $ fromMaybe integerType t
typecheckExpr (VarExpr nUntyped (Just t'Untyped)) t = do
  n <- typecheckVarName nUntyped
  t' <- typecheckType t'Untyped
  for_ t $ partialTypeEqual (VarType n)
  for_ t $ flip partialTypeEqual (RealType t')
  return $ TypedVarExpr n (RealType t')
typecheckExpr (VarExpr nUntyped Nothing) t = do
  n <- typecheckVarName nUntyped
  for_ t $ partialTypeEqual (VarType n)
  return $ TypedVarExpr n (VarType n)
typecheckExpr (ColExpr c) t = do
  t' <- getColumnType c
  for_ t $ partialTypeEqual (RealType t')
  return $ TypedColExpr c t'
typecheckExpr (AndExpr e1 e2) t = do
  for_ t $ partialTypeEqual boolType
  TypedAndExpr <$> typecheckExpr e1 (Just boolType) <*> typecheckExpr e2 (Just boolType)
typecheckExpr (EqExpr e1 e2) t = do
  for_ t $ partialTypeEqual boolType
  t1 <- typecheckExpr e1 Nothing
  t2 <- typecheckExpr e2 (Just (typedExprType t1))
  return $ TypedEqExpr t1 t2
typecheckExpr (FunExpr n e) t = do
  f@(TypedFunction (FunName strName) args ret) <- getTypedFunction n
  when (length args /= length e) $ fail $ arityMismatch (mkName strName) args e -- TODO: this is a hack
  e' <- zipWithM (\ex etyp -> typecheckExpr ex (Just (RealType etyp))) e args
  for_ t $ flip partialTypeEqual (RealType ret)
  return $ TypedFunExpr f e'
typecheckExpr (InExpr e1 e2) t = do
  for_ t $ partialTypeEqual boolType
  t1 <- typecheckExpr e1 Nothing
  t2 <- typecheckListExpr e2 (typedExprType t1)
  return $ TypedInExpr t1 t2

data DefColumnsCol = DefColumnsCol UpdateOnDuplicateKey ColumnName | DefColumnsType TypeName deriving (Show, Eq)

newtype DefColumns = DefColumns (M.Map TypeName (TableName, [DefColumnsCol])) deriving (Show, Eq)

findImplicitComplexExpr :: DefColumns -> TypeName -> Maybe ComplexExpression
findImplicitComplexExpr (DefColumns dc) t = (\(tn,cols) -> ComplexExpr (Just t) (map (helper tn) cols)) <$> dc M.!? t
  where
    helper tn (DefColumnsCol _ cn) = SimpleExpr $ ColExpr $ FullColumnName (Just tn) cn
    helper _ (DefColumnsType ref) = ImplicitComplexExpr ref

typeConstructorArgs :: MonadQ m => Name -> m [PartialType]
typeConstructorArgs n = do
  (TyConI dec) <- liftQ $ reify n
  case dec of
    DataD _ _ [] _ [c] _ -> deconstructConstructor c
    DataD {} -> fail "Only datatypes with a single constructor and no type arguments are supported!"
    NewtypeD _ _ [] _ c _ -> deconstructConstructor c
    NewtypeD {} -> fail "Only datatypes with a single constructor and no type arguments are supported!"
    _ -> fail $ "Unexpected declaration when reifying: " ++ show n ++ "!"
  where
    deconstructConstructor (NormalC _ s) = return $ map (deconstructType . snd) s
    deconstructConstructor c = fail $ "Unsupported constructor: " ++ show c ++ "!"
    deconstructMultiApp (AppT a b) = let (f,args) = deconstructMultiApp a in (f, args ++ [b])
    deconstructMultiApp t = (t,[])
    deconstructType (deconstructMultiApp -> (TupleT _, vals)) = TupleType (map deconstructType vals)
    deconstructType t = RealType t

arityMismatch :: Name -> [a] -> [b] -> String
arityMismatch n a b = "Arity doesn't match: " ++ show n ++ " expected: " ++ show (length a) ++ " arguments, but got: " ++ show (length b) ++ "!"

tupleLengthMismatch :: [a] -> [b] -> String
tupleLengthMismatch a b = "Tuple length doesn't match, expected: " ++ show (length a) ++ ", but got: " ++ show (length b) ++ "!"

typecheckComplexExpr :: DefColumns -> ComplexExpression -> Maybe PartialType -> Typecheck TypedComplexExpression
typecheckComplexExpr _ (SimpleExpr e) t' = TypedSimpleExpr <$> typecheckExpr e t'

typecheckComplexExpr dc (ComplexExpr (Just nUntyped) s) t = do
  n <- typecheckTypeName nUntyped
  for_ t $ flip partialTypeEqual (RealType (ConT n))
  args <- typeConstructorArgs n
  when (length args /= length s) $ fail $ arityMismatch n args s
  TypedComplexExpr (RealType (ConT n)) <$> zipWithM (\e a -> typecheckComplexExpr dc e (Just a)) s args

typecheckComplexExpr dc (ComplexExpr Nothing s) Nothing = do
  es <- mapM (\e -> typecheckComplexExpr dc e Nothing) s
  return $ TypedComplexExpr (TupleType $ map typedComplexExprType es) es
typecheckComplexExpr dc (ComplexExpr Nothing s) (Just (RealType t)) = do
  n <- case t of
    ConT n -> return n
    _ -> fail $ "Bad type expected: " ++ show t ++ "!"
  args <- typeConstructorArgs n
  when (length args /= length s) $ fail $ arityMismatch n args s
  TypedComplexExpr (RealType t) <$> zipWithM (\e a -> typecheckComplexExpr dc e (Just a)) s args
typecheckComplexExpr dc (ComplexExpr Nothing s) (Just (TupleType t)) = do
  when (length s /= length t) $ fail $ tupleLengthMismatch t s
  TypedComplexExpr (TupleType t) <$> zipWithM (\e a -> typecheckComplexExpr dc e (Just a)) s t
typecheckComplexExpr _ (ComplexExpr Nothing _) (Just (VarType t)) = fail $ "Not enough information about variable " ++ show t ++ " to guess the type!"

typecheckComplexExpr dc (ImplicitComplexExpr nUntyped) t = do
  n <- typecheckTypeName nUntyped
  case findImplicitComplexExpr dc nUntyped of
    Nothing -> fail $ "Default columns not found: " ++ show n
    Just e -> typecheckComplexExpr dc e t

typecheckTableName :: [(TableName, [(ColumnName, ParsedType)])] -> AliasedTable -> EarlyTypecheck Schema
typecheckTableName schemaAll (AliasedTable t a) = Schema <$> case lookup t schemaAll of
    Nothing -> fail $ "Table not found: " ++ ppTableName t ++ "!"
    Just s -> mapM (\(cn, ct) -> (fromMaybe t a, cn,) <$> typecheckType ct) s

typecheckJoinTables :: [(TableName, [(ColumnName, ParsedType)])] -> SqlFuns -> JoinTables -> EarlyTypecheck Schema
typecheckJoinTables schemaAll funs (JoinTables x xs) = do
  (Schema firstTable) <- typecheckTableName schemaAll x
  Schema <$> foldlM helper firstTable xs
 where
  helper schema (t, TableJoinOn c1 c2) = do
    (Schema s) <- typecheckTableName schemaAll t
    t1 <- runReaderT (getColumnType (FullColumnName Nothing c1)) (Schema s, funs)
    t2 <- runReaderT (getColumnType c2) (Schema schema, funs)
    unless (t1 == t2) $ fail $ couldntMatchTypes (RealType t1) (RealType t2)
    return $ schema ++ s

typecheckWhereClause :: WhereClause -> Typecheck TypedWhereClause
typecheckWhereClause (WhereClause e) = TypedWhereClause <$> mapM (`typecheckExpr` Just boolType) e

typecheckSelectQuery :: [(TableName, [(ColumnName, ParsedType)])] -> SqlFuns -> DefColumns -> Maybe PartialType -> SelectQuery -> EarlyTypecheck TypedSelectQuery
typecheckSelectQuery schemaAll funs dc expectedType (SelectQuery s t w) = do
  schema <- typecheckJoinTables schemaAll funs t
  flip runReaderT (schema, funs) $ do
    s' <- mapM (\e -> typecheckComplexExpr dc e Nothing) s
    for_ expectedType $ flip partialTypeEqual (implicitTupleType $ map typedComplexExprType s')
    TypedSelectQuery s' t <$> typecheckWhereClause w

typecheckValuesRow :: SqlFuns -> PartialType -> ValuesRow -> EarlyTypecheck TypedValuesRow
typecheckValuesRow funs t (ValuesRowExpr e) = fmap TypedValuesRowExpr . flip runReaderT (Schema [], funs) $ typecheckComplexExpr (DefColumns M.empty) (implicitTupleExpr e) (Just t)
typecheckValuesRow _ t (ValuesRowVar nUntyped r) = do
  n <- typecheckVarName nUntyped
  case r of
    ValuesRowSingle -> partialTypeEqual t (VarType n)
    ValuesRowMany -> do
      n' <- liftQ $ newName (nameBase n)
      tell [VarIsList n n']
      partialTypeEqual t (VarType n')
  return $ TypedValuesRowVar n r

typecheckInsertSouce :: [(TableName, [(ColumnName, ParsedType)])] -> SqlFuns -> DefColumns -> PartialType -> InsertSource -> EarlyTypecheck TypedInsertSource
typecheckInsertSouce _ funs _ t (ValuesSource rows) = TypedValuesSource <$> mapM (typecheckValuesRow funs t) rows
typecheckInsertSouce schemaAll funs dc t (SelectSource q) = TypedSelectSource <$> typecheckSelectQuery schemaAll funs dc (Just t) q

combineTypedInsertTargets :: [(TypedInsertTarget, [ColumnName])] -> ([TypedInsertTarget], [ColumnName])
combineTypedInsertTargets xs = (map fst xs, snd =<< xs)

findImplicitInsertTarget :: DefColumns -> TypeName -> Maybe InsertTarget
findImplicitInsertTarget (DefColumns dc) t = (\(_,cols) -> ComplexTarget (Just t) (map helper cols)) <$> dc M.!? t -- TODO: this doesnt check for table!
  where
    helper (DefColumnsCol u' cn) = ColTarget u' cn
    helper (DefColumnsType ref) = ImplicitComplexTarget ref

typecheckInsertTarget :: DefColumns -> Maybe PartialType -> InsertTarget -> Typecheck (TypedInsertTarget, [ColumnName])
typecheckInsertTarget _ t' (ColTarget u c) = do
  t <- getColumnType (FullColumnName Nothing c)
  for_ t' $ flip partialTypeEqual (RealType t)
  return (TypedColTarget c t, [c | u == DoUpdate])

typecheckInsertTarget dc t' (ComplexTarget (Just nUntyped) s) = do
  n <- typecheckTypeName nUntyped
  for_ t' $ flip partialTypeEqual (RealType (ConT n))
  args <- typeConstructorArgs n
  (s', c) <- combineTypedInsertTargets <$> zipWithM (typecheckInsertTarget dc . Just) args s
  return (TypedComplexTarget (RealType (ConT n)) s', c)

typecheckInsertTarget dc Nothing (ComplexTarget Nothing s) = do
  (s', c) <- combineTypedInsertTargets <$> mapM (typecheckInsertTarget dc Nothing) s
  return (TypedComplexTarget (TupleType $ map typedInsertTargetType s') s', c)
typecheckInsertTarget dc (Just (RealType t)) (ComplexTarget Nothing s) = do
  n <- case t of
    ConT n -> return n
    _ -> fail $ "Bad type expected: " ++ show t ++ "!"
  args <- typeConstructorArgs n
  when (length args /= length s) $ fail $ arityMismatch n args s
  (s', c) <- combineTypedInsertTargets <$> zipWithM (typecheckInsertTarget dc . Just) args s
  return (TypedComplexTarget (RealType t) s', c)
typecheckInsertTarget dc (Just (TupleType t)) (ComplexTarget Nothing s) = do
  when (length t /= length s) $ fail $ tupleLengthMismatch t s
  (s', c) <- combineTypedInsertTargets <$> zipWithM (typecheckInsertTarget dc . Just) t s
  return (TypedComplexTarget (TupleType t) s', c)
typecheckInsertTarget _ (Just (VarType t)) (ComplexTarget Nothing _) = fail $ "Not enough information about variable " ++ show t ++ " to guess the type!"

typecheckInsertTarget dc t (ImplicitComplexTarget nUntyped) = do
  n <- typecheckTypeName nUntyped
  case findImplicitInsertTarget dc nUntyped of
    Nothing -> fail $ "Default columns not found: " ++ show n
    Just e -> typecheckInsertTarget dc t e

typecheckInsertQuery :: [(TableName, [(ColumnName, ParsedType)])] -> SqlFuns -> DefColumns -> InsertQuery -> EarlyTypecheck TypedInsertQuery
typecheckInsertQuery schemaAll funs dc (InsertQuery tn t s) = do
  schema <- typecheckTableName schemaAll (AliasedTable tn Nothing)
  (t', u) <- flip runReaderT (schema, funs) $ combineTypedInsertTargets <$> mapM (typecheckInsertTarget dc Nothing) t
  s' <- typecheckInsertSouce schemaAll funs dc (implicitTupleType $ map typedInsertTargetType t') s
  return $ TypedInsertQuery tn t' s' (UpdateOnDuplicateList u)


typecheckAnyQuery :: [(TableName, [(ColumnName, ParsedType)])] -> SqlFuns -> DefColumns -> AnyQuery -> EarlyTypecheck TypedAnyQuery
typecheckAnyQuery schema funs dc (SelectAnyQuery q) = TypedSelectAnyQuery <$> typecheckSelectQuery schema funs dc Nothing q
typecheckAnyQuery schema funs dc (InsertAnyQuery q) = TypedInsertAnyQuery <$> typecheckInsertQuery schema funs dc q