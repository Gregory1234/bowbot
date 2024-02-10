{-# LANGUAGE TemplateHaskellQuotes #-}

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
import Control.Monad

data TypecheckConstraint
  = NameHasType Type Name
  | NameFitsType Name Type
  | NameTypesEq Name Name
  | NameHasTypeLax Type Name
  | NameTypesEqLax Name Name
  | TypeIsString Type
  | TypeIsInt Type
  | VarIsString Name
  | VarIsInt Name
  | VarIsTuple Name [Name]
  | VarIsList Name Name
  | VarNotNull Name
  | VarIsMaybe Name
  deriving (Show, Eq)

class (MonadIO m, MonadFail m) => MonadQ m where liftQ :: Q a -> m a
instance MonadQ EarlyTypecheck where liftQ = lift
instance MonadQ Typecheck where liftQ = lift . lift

data ColumnInfo = ColumnInfo { columnInfoType :: ParsedType, columnInfoAI :: Bool }

newtype Schema = Schema [(TableName, ColumnName, Either String Type)] deriving (Show, Eq)

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

partialTypeNotNull :: PartialType -> Typecheck ()
partialTypeNotNull (RealType t@(AppT (ConT m) _)) | m == ''Maybe = fail $ "Type is nullable: " ++ show t ++ "!"
partialTypeNotNull (RealType _) = pure ()
partialTypeNotNull (VarType n) = tell [VarNotNull n]
partialTypeNotNull (TupleType _) = pure ()

removeMaybeFromType :: PartialType -> PartialType
removeMaybeFromType (RealType (AppT (ConT m) v)) | m == ''Maybe = RealType v
removeMaybeFromType x = x

partialTypeIsMaybe :: (MonadWriter [TypecheckConstraint] m, MonadFail m) => PartialType -> m ()
partialTypeIsMaybe (RealType (AppT (ConT m) _)) | m == ''Maybe = pure ()
partialTypeIsMaybe (RealType t) = fail $ "Type is not nullable: " ++ show t ++ "!"
partialTypeIsMaybe (VarType n) = tell [VarIsMaybe n]
partialTypeIsMaybe (TupleType _) = fail "Tuple type is never nullable!"

couldntMatchTypes :: PartialType -> PartialType -> String
couldntMatchTypes t1 t2 = "Couldn't match types: " ++ show t1 ++ " and " ++ show t2 -- TODO: nicer errors?

data EqStrictness = EqStrict | EqLax deriving (Show, Eq)

data StrictnessType = StrictnessType { strictnessTypeStrictness :: EqStrictness, strictnessTypeType :: PartialType } deriving (Show, Eq)

partialTypeEqual :: (MonadWriter [TypecheckConstraint] m, MonadQ m) => StrictnessType -> PartialType -> m ()
partialTypeEqual (StrictnessType s (RealType t1)) (RealType t2)
  | t1 == t2 = pure ()
  | t1 == AppT (ConT ''Maybe) t2 = pure ()
  | s == EqLax, AppT (ConT ''Maybe) t1 == t2 = pure ()
  | otherwise = fail $ couldntMatchTypes (RealType t1) (RealType t2)
partialTypeEqual (StrictnessType EqStrict (RealType t1)) (VarType t2) = tell [NameHasType t1 t2]
partialTypeEqual (StrictnessType EqLax (RealType t1)) (VarType t2) = tell [NameHasTypeLax t1 t2]
partialTypeEqual (StrictnessType EqStrict (VarType t1)) (RealType t2) = tell [NameFitsType t1 t2]
partialTypeEqual (StrictnessType EqLax (VarType t1)) (RealType t2) = tell [NameHasTypeLax t2 t1]
partialTypeEqual (StrictnessType EqStrict (VarType t1)) (VarType t2) = tell [NameTypesEq t1 t2]
partialTypeEqual (StrictnessType EqLax (VarType t1)) (VarType t2) = tell [NameTypesEqLax t1 t2]
partialTypeEqual (StrictnessType s (TupleType t1)) (TupleType t2)
  | length t1 /= length t2 = fail $ tupleLengthMismatch t1 t2
  | otherwise = zipWithM_ partialTypeEqual (StrictnessType s <$> t1) t2
partialTypeEqual (StrictnessType _ (RealType t1)) (TupleType t2) = fail $ couldntMatchTypes (RealType t1) (TupleType t2)
partialTypeEqual (StrictnessType _ (TupleType t1)) (RealType t2) = fail $ couldntMatchTypes (TupleType t1) (RealType t2)
partialTypeEqual (StrictnessType s (VarType t1)) (TupleType t2) = do
  ns <- liftQ $ mapM (const (newName (nameBase t1))) t2
  tell [VarIsTuple t1 ns]
  zipWithM_ (partialTypeEqual . StrictnessType s . VarType) ns t2
partialTypeEqual (StrictnessType s (TupleType t1)) (VarType t2) = do
  ns <- liftQ $ mapM (const (newName (nameBase t2))) t1
  tell [VarIsTuple t2 ns]
  zipWithM_ (\t -> partialTypeEqual (StrictnessType s t) . VarType) t1 ns

partialTypeEqual' :: (MonadWriter [TypecheckConstraint] m, MonadQ m) => Maybe StrictnessType -> Maybe PartialType -> m ()
partialTypeEqual' (Just t1) (Just t2) = partialTypeEqual t1 t2
partialTypeEqual' _ _ = pure ()

getColumnType :: FullColumnName -> Typecheck Type
getColumnType fcn@(FullColumnName t c) = do
  (Schema s, _) <- ask
  case filter (\(t',c',_) -> c == c' && (isNothing t || t == Just t')) s of
    [(_,_,Right typ)] -> return typ
    [(_,_,Left e)] -> fail e
    [] -> fail $ "Couldn't find column: " ++ ppFullColumnName fcn ++ "!"
    _ -> fail $ "Ambigous column: " ++ ppFullColumnName fcn ++ "!"

forceEither :: MonadFail m => Either String a -> m a
forceEither (Left e) = fail e
forceEither (Right t) = return t

typecheckType' :: MonadQ m => ParsedType -> m (Either String Type)
typecheckType' (ParsedType t) = do
  realTypes <- fmap (map (\x -> if x == mkName "List" then ListT else ConT x)) . sequence <$> mapM typecheckTypeName' t
  return $ foldl1 AppT <$> realTypes

typecheckType :: MonadQ m => ParsedType -> m Type
typecheckType = typecheckType' >=> forceEither

typecheckTypeName' :: MonadQ m => TypeName -> m (Either String Name)
typecheckTypeName' (TypeName "List") = return $ Right (mkName "List")
typecheckTypeName' (TypeName n) = do
  n' <- liftQ $ lookupTypeName n
  case n' of
    Nothing -> return $ Left $ "Type not found: " ++ n ++ "!"
    Just n'' -> return $ Right n''

typecheckTypeName :: MonadQ m => TypeName -> m Name
typecheckTypeName = typecheckTypeName' >=> forceEither

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

typecheckListExpr :: ListExpression -> StrictnessType -> Typecheck TypedListExpression
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
  

typecheckExpr :: Expression -> Maybe StrictnessType -> Typecheck TypedExpression
typecheckExpr (StringExpr s t1Untyped) t2 = do
  t1 <- mapM typecheckType t1Untyped
  let t = RealType <$> t1 <|> strictnessTypeType <$> t2
  partialTypeEqual' t2 (RealType <$> t1)
  for_ t partialTypeIsString
  return $ TypedStringExpr s $ fromMaybe textType t

typecheckExpr (IntExpr s t1Untyped) t2 = do
  t1 <- mapM typecheckType t1Untyped
  let t = RealType <$> t1 <|> strictnessTypeType <$> t2
  partialTypeEqual' t2 (RealType <$> t1)
  for_ t partialTypeIsInt
  return $ TypedIntExpr s $ fromMaybe integerType t

typecheckExpr (VarExpr nUntyped (Just t'Untyped)) t = do
  n <- typecheckVarName nUntyped
  t' <- typecheckType t'Untyped
  partialTypeEqual' t (Just $ RealType t')
  partialTypeEqual (StrictnessType EqStrict $ RealType t') (VarType n)
  return $ TypedVarExpr n (RealType t')
typecheckExpr (VarExpr nUntyped Nothing) t = do
  n <- typecheckVarName nUntyped
  partialTypeEqual' t (Just $ VarType n)
  return $ TypedVarExpr n (VarType n)

typecheckExpr (ColExpr c) t = do
  t' <- getColumnType c
  partialTypeEqual' t (Just $ RealType t')
  return $ TypedColExpr c t'

typecheckExpr (AndExpr e1 e2) t = do
  partialTypeEqual' t (Just boolType)
  TypedAndExpr <$> typecheckExpr e1 (Just $ StrictnessType EqStrict boolType) <*> typecheckExpr e2 (Just $ StrictnessType EqStrict boolType)

typecheckExpr (EqExpr e1 op e2@(NullExpr _)) t = do
  partialTypeEqual' t (Just boolType)
  t1 <- typecheckExpr e1 Nothing
  t2 <- typecheckExpr e2 (Just $ StrictnessType EqLax $ typedExprType t1)
  return $ TypedEqExpr t1 op t2
typecheckExpr (EqExpr e1 op e2) t = do
  partialTypeEqual' t (Just boolType)
  t1 <- typecheckExpr e1 Nothing
  t2 <- typecheckExpr e2 (Just $ StrictnessType EqStrict $ removeMaybeFromType $ typedExprType t1)
  partialTypeNotNull $ typedExprType t2
  return $ TypedEqExpr t1 op t2

typecheckExpr (CompExpr e1 op e2) t = do
  partialTypeEqual' t (Just boolType)
  t1 <- typecheckExpr e1 Nothing
  partialTypeNotNull $ typedExprType t1
  t2 <- typecheckExpr e2 (Just $ StrictnessType EqStrict $ typedExprType t1)
  partialTypeNotNull $ typedExprType t2
  return $ TypedCompExpr t1 op t2

typecheckExpr (FunExpr n e) t = do
  f@(TypedFunction (FunName strName) args ret) <- getTypedFunction n
  when (length args /= length e) $ fail $ arityMismatch (mkName strName) args e -- TODO: this is a hack
  e' <- zipWithM (\etyp ex -> typecheckExpr ex (Just $ StrictnessType EqStrict $ RealType etyp)) args e
  partialTypeEqual' t (Just $ RealType ret)
  return $ TypedFunExpr f e'

typecheckExpr (InExpr e1 e2) t = do
  partialTypeEqual' t (Just boolType)
  t1 <- typecheckExpr e1 Nothing
  t2 <- typecheckListExpr e2 (StrictnessType EqLax $ typedExprType t1)
  return $ TypedInExpr t1 t2

typecheckExpr (NotInExpr e1 e2) t = do
  partialTypeEqual' t (Just boolType)
  t1 <- typecheckExpr e1 Nothing
  t2 <- typecheckListExpr e2 (StrictnessType EqLax $ typedExprType t1)
  return $ TypedNotInExpr t1 t2

typecheckExpr (OverrideExpr e tUntyped) t' = do
  t <- typecheckType tUntyped
  partialTypeEqual' t' (Just $ RealType t)
  TypedOverrideExpr <$> typecheckExpr e Nothing <*> pure t

typecheckExpr (NullExpr t1Untyped) t2 = do
  t1 <- mapM typecheckType t1Untyped
  let maybeT1 = RealType . AppT (ConT ''Maybe) <$> t1
  partialTypeEqual' t2 maybeT1
  let t = maybeT1 <|> strictnessTypeType <$> t2
  for_ t2 $ \t2' -> when (strictnessTypeStrictness t2' == EqStrict) $ partialTypeIsMaybe $ strictnessTypeType t2'
  case t of
    Nothing -> fail "Couldn't deduce type of NULL!"
    Just t' -> return $ TypedNullExpr t'

data DefColumnsCol = DefColumnsCol UpdateOnDuplicateKey ColumnName | DefColumnsType TypeName | DefColumnsTuple [DefColumnsCol] deriving (Show, Eq)

newtype DefColumns = DefColumns (M.Map TypeName (TableName, [DefColumnsCol])) deriving (Show, Eq)

findImplicitComplexExpr :: DefColumns -> TypeName -> Maybe ComplexExpression
findImplicitComplexExpr (DefColumns dc) t = (\(tn,cols) -> ComplexExpr (Just t) (map (helper tn) cols)) <$> dc M.!? t
  where
    helper tn (DefColumnsCol _ cn) = SimpleExpr $ ColExpr $ FullColumnName (Just tn) cn
    helper _ (DefColumnsType ref) = ImplicitComplexExpr ref
    helper tn (DefColumnsTuple tup) = ComplexExpr Nothing $ map (helper tn) tup

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
    deconstructConstructor (RecC _ s) = return $ map (deconstructType . (\(_,_,t) -> t)) s
    deconstructConstructor c = fail $ "Unsupported constructor: " ++ show c ++ "!"
    deconstructMultiApp (AppT a b) = let (f,args) = deconstructMultiApp a in (f, args ++ [b])
    deconstructMultiApp t = (t,[])
    deconstructType (deconstructMultiApp -> (TupleT _, vals)) = TupleType (map deconstructType vals)
    deconstructType t = RealType t

arityMismatch :: Name -> [a] -> [b] -> String
arityMismatch n a b = "Arity doesn't match: " ++ show n ++ " expected: " ++ show (length a) ++ " arguments, but got: " ++ show (length b) ++ "!"

tupleLengthMismatch :: [a] -> [b] -> String
tupleLengthMismatch a b = "Tuple length doesn't match, expected: " ++ show (length a) ++ ", but got: " ++ show (length b) ++ "!"

typecheckComplexExpr :: DefColumns -> ComplexExpression -> Maybe StrictnessType -> Typecheck TypedComplexExpression
typecheckComplexExpr _ (SimpleExpr e) t' = TypedSimpleExpr <$> typecheckExpr e t'

typecheckComplexExpr dc (ComplexExpr (Just nUntyped) s) t = do
  n <- typecheckTypeName nUntyped
  partialTypeEqual' t (Just $ RealType (ConT n))
  args <- typeConstructorArgs n
  when (length args /= length s) $ fail $ arityMismatch n args s
  TypedComplexExpr (RealType (ConT n)) <$> zipWithM (\e a -> typecheckComplexExpr dc e (Just $ StrictnessType EqStrict a)) s args

typecheckComplexExpr dc (ComplexExpr Nothing s) Nothing = do
  es <- mapM (\e -> typecheckComplexExpr dc e Nothing) s
  return $ TypedComplexExpr (TupleType $ map typedComplexExprType es) es
typecheckComplexExpr dc (ComplexExpr Nothing s) (Just (StrictnessType _ (RealType t))) = do
  n <- case t of
    ConT n -> return n
    _ -> fail $ "Bad type expected: " ++ show t ++ "!"
  args <- typeConstructorArgs n
  when (length args /= length s) $ fail $ arityMismatch n args s
  TypedComplexExpr (RealType t) <$> zipWithM (\e a -> typecheckComplexExpr dc e (Just $ StrictnessType EqStrict a)) s args
typecheckComplexExpr dc (ComplexExpr Nothing s) (Just (StrictnessType str (TupleType t))) = do
  when (length s /= length t) $ fail $ tupleLengthMismatch t s
  TypedComplexExpr (TupleType t) <$> zipWithM (\e a -> typecheckComplexExpr dc e (Just $ StrictnessType str a)) s t
typecheckComplexExpr _ (ComplexExpr Nothing _) (Just (StrictnessType _ (VarType t))) = fail $ "Not enough information about variable " ++ show t ++ " to guess the type!"

typecheckComplexExpr dc (ImplicitComplexExpr nUntyped) t = do
  n <- typecheckTypeName nUntyped
  case findImplicitComplexExpr dc nUntyped of
    Nothing -> fail $ "Default columns not found: " ++ show n
    Just e -> typecheckComplexExpr dc e t

typecheckTableName :: [(TableName, [(ColumnName, ColumnInfo)])] -> AliasedTable -> EarlyTypecheck Schema
typecheckTableName schemaAll (AliasedTable t a) = Schema <$> case lookup t schemaAll of
    Nothing -> fail $ "Table not found: " ++ ppTableName t ++ "!"
    Just s -> mapM (\(cn, ColumnInfo {..}) -> (fromMaybe t a, cn,) <$> typecheckType' columnInfoType) s

typecheckJoinTables :: [(TableName, [(ColumnName, ColumnInfo)])] -> SqlFuns -> JoinTables -> EarlyTypecheck Schema
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
typecheckWhereClause (WhereClause e) = TypedWhereClause <$> mapM (`typecheckExpr` Just (StrictnessType EqStrict boolType)) e

typecheckSelectQuery :: [(TableName, [(ColumnName, ColumnInfo)])] -> SqlFuns -> DefColumns -> Maybe StrictnessType -> SelectQuery -> EarlyTypecheck TypedSelectQuery
typecheckSelectQuery schemaAll funs dc expectedType (SelectQuery s t w) = do
  schema <- typecheckJoinTables schemaAll funs t
  flip runReaderT (schema, funs) $ do
    s' <- typecheckComplexExpr dc s expectedType
    TypedSelectQuery s' t <$> typecheckWhereClause w

typecheckValuesRow :: SqlFuns -> StrictnessType -> ValuesRow -> EarlyTypecheck TypedValuesRow
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

typecheckInsertSource :: [(TableName, [(ColumnName, ColumnInfo)])] -> SqlFuns -> DefColumns -> StrictnessType -> InsertSource -> EarlyTypecheck TypedInsertSource
typecheckInsertSource _ funs _ t (ValuesSource rows) = TypedValuesSource <$> mapM (typecheckValuesRow funs t) rows
typecheckInsertSource schemaAll funs dc t (SelectSource q) = TypedSelectSource <$> typecheckSelectQuery schemaAll funs dc (Just t) q

combineTypedInsertTargets :: [(TypedInsertTarget, [ColumnName])] -> ([TypedInsertTarget], [ColumnName])
combineTypedInsertTargets xs = (map fst xs, snd =<< xs)

findImplicitInsertTarget :: DefColumns -> TypeName -> Maybe InsertTarget
findImplicitInsertTarget (DefColumns dc) t = (\(_,cols) -> ComplexTarget (Just t) (map helper cols)) <$> dc M.!? t -- TODO: this doesnt check for table!
  where
    helper (DefColumnsCol u' cn) = ColTarget u' cn
    helper (DefColumnsType ref) = ImplicitComplexTarget ref
    helper (DefColumnsTuple tup) = ComplexTarget Nothing $ map helper tup

typecheckInsertTarget :: DefColumns -> Maybe StrictnessType -> InsertTarget -> Typecheck (TypedInsertTarget, [ColumnName])
typecheckInsertTarget _ t' (ColTarget u c) = do
  t <- getColumnType (FullColumnName Nothing c)
  for_ t' $ flip partialTypeEqual (RealType t)
  return (TypedColTarget c t, [c | u == DoUpdate])

typecheckInsertTarget dc t' (ComplexTarget (Just nUntyped) s) = do
  n <- typecheckTypeName nUntyped
  partialTypeEqual' t' (Just $ RealType (ConT n))
  args <- typeConstructorArgs n
  (s', c) <- combineTypedInsertTargets <$> zipWithM (typecheckInsertTarget dc . Just . StrictnessType EqStrict) args s
  return (TypedComplexTarget (RealType (ConT n)) s', c)

typecheckInsertTarget dc Nothing (ComplexTarget Nothing s) = do
  (s', c) <- combineTypedInsertTargets <$> mapM (typecheckInsertTarget dc Nothing) s
  return (TypedComplexTarget (TupleType $ map typedInsertTargetType s') s', c)
typecheckInsertTarget dc (Just (StrictnessType _ (RealType t))) (ComplexTarget Nothing s) = do
  n <- case t of
    ConT n -> return n
    _ -> fail $ "Bad type expected: " ++ show t ++ "!"
  args <- typeConstructorArgs n
  when (length args /= length s) $ fail $ arityMismatch n args s
  (s', c) <- combineTypedInsertTargets <$> zipWithM (typecheckInsertTarget dc . Just . StrictnessType EqStrict) args s
  return (TypedComplexTarget (RealType t) s', c)
typecheckInsertTarget dc (Just (StrictnessType str (TupleType t))) (ComplexTarget Nothing s) = do
  when (length t /= length s) $ fail $ tupleLengthMismatch t s
  (s', c) <- combineTypedInsertTargets <$> zipWithM (typecheckInsertTarget dc . Just . StrictnessType str) t s
  return (TypedComplexTarget (TupleType t) s', c)
typecheckInsertTarget _ (Just (StrictnessType _ (VarType t))) (ComplexTarget Nothing _) = fail $ "Not enough information about variable " ++ show t ++ " to guess the type!"

typecheckInsertTarget dc t (ImplicitComplexTarget nUntyped) = do
  n <- typecheckTypeName nUntyped
  case findImplicitInsertTarget dc nUntyped of
    Nothing -> fail $ "Default columns not found: " ++ show n
    Just e -> typecheckInsertTarget dc t e

typecheckSimpleInsertSource :: SqlFuns -> StrictnessType -> SimpleInsertSource -> EarlyTypecheck TypedSimpleInsertSource
typecheckSimpleInsertSource funs t (SimpleValuesSource e) = fmap TypedSimpleValuesSource . flip runReaderT (Schema [], funs) $ typecheckComplexExpr (DefColumns M.empty) (implicitTupleExpr e) (Just t)

typecheckAutoIncrement :: [(TableName, [(ColumnName, ColumnInfo)])] -> TableName -> Maybe ParsedType -> EarlyTypecheck Type
typecheckAutoIncrement schemaAll tn Nothing = case lookup tn schemaAll of
  Nothing -> fail $ "Table not found: " ++ ppTableName tn ++ "!"
  Just cols -> case filter (columnInfoAI . snd) cols of
    [] -> fail $ "Table has no auto-increment column: " ++ ppTableName tn ++ "!"
    [(_, ColumnInfo {..})] -> typecheckType columnInfoType
    _ -> fail $ "Table has multiple auto-increment columns: " ++ ppTableName tn ++ "!"
typecheckAutoIncrement _ _ (Just t) = typecheckType t

typecheckInsertQuery :: [(TableName, [(ColumnName, ColumnInfo)])] -> SqlFuns -> DefColumns -> InsertQuery -> EarlyTypecheck TypedInsertQuery
typecheckInsertQuery schemaAll funs dc (InsertQuery tn t s) = do
  schema <- typecheckTableName schemaAll (AliasedTable tn Nothing)
  (t', u) <- flip runReaderT (schema, funs) $ typecheckInsertTarget dc Nothing t
  s' <- typecheckInsertSource schemaAll funs dc (StrictnessType EqStrict $ typedInsertTargetType t') s
  return $ TypedInsertQuery tn t' s' (UpdateOnDuplicateList u)
typecheckInsertQuery schemaAll funs dc (InsertAIQuery ait tn t s) = do
  schema <- typecheckTableName schemaAll (AliasedTable tn Nothing)
  ait' <- typecheckAutoIncrement schemaAll tn ait
  (t', u) <- flip runReaderT (schema, funs) $ typecheckInsertTarget dc Nothing t
  s' <- typecheckSimpleInsertSource funs (StrictnessType EqStrict $ typedInsertTargetType t') s
  return $ TypedInsertAIQuery ait' tn t' s' (UpdateOnDuplicateList u)

typecheckColumnUpdate :: ColumnUpdate -> Typecheck TypedColumnUpdate
typecheckColumnUpdate (ColumnUpdate c e) = do
  t <- getColumnType c
  TypedColumnUpdate c <$> typecheckExpr e (Just $ StrictnessType EqStrict $ RealType t)

typecheckUpdateQuery :: [(TableName, [(ColumnName, ColumnInfo)])] -> SqlFuns -> UpdateQuery -> EarlyTypecheck TypedUpdateQuery
typecheckUpdateQuery schemaAll funs (UpdateQuery t u w) = do
  schema <- typecheckJoinTables schemaAll funs t
  flip runReaderT (schema, funs) $ TypedUpdateQuery t <$> mapM typecheckColumnUpdate u <*> typecheckWhereClause w

typecheckDeleteQuery :: [(TableName, [(ColumnName, ColumnInfo)])] -> SqlFuns -> DeleteQuery -> EarlyTypecheck TypedDeleteQuery
typecheckDeleteQuery schemaAll funs (DeleteQuery t w) = do
  schema <- typecheckTableName schemaAll (AliasedTable t Nothing)
  flip runReaderT (schema, funs) $ TypedDeleteQuery t <$> typecheckWhereClause w

typecheckAnyQuery :: [(TableName, [(ColumnName, ColumnInfo)])] -> SqlFuns -> DefColumns -> AnyQuery -> EarlyTypecheck TypedAnyQuery
typecheckAnyQuery schema funs dc (SelectAnyQuery q) = TypedSelectAnyQuery <$> typecheckSelectQuery schema funs dc Nothing q
typecheckAnyQuery schema funs dc (InsertAnyQuery q) = TypedInsertAnyQuery <$> typecheckInsertQuery schema funs dc q
typecheckAnyQuery schema funs _ (UpdateAnyQuery q) = TypedUpdateAnyQuery <$> typecheckUpdateQuery schema funs q
typecheckAnyQuery schema funs _ (DeleteAnyQuery q) = TypedDeleteAnyQuery <$> typecheckDeleteQuery schema funs q