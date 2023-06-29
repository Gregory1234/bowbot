{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module BowBot.DB.Typed(
  module BowBot.DB.Typed, module BowBot.DB.Basic
) where

import BowBot.Utils
import Database.MySQL.Simple.Types (Query(..))
import BowBot.DB.Basic
import Data.Int (Int64)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Proxy (Proxy(..))

data NoOutput

newtype TypedQuery i o = TypedQuery { getTypedQuery :: Query }

type TypedQuery' i = TypedQuery i NoOutput

queryLogT :: (QueryParams q, QueryResults r, MonadIOReader m rd, Has Connection rd) => TypedQuery q r -> q -> m [r]
queryLogT = queryLog . getTypedQuery

queryLogT_ :: (QueryResults r, MonadIOReader m rd, Has Connection rd) => TypedQuery () r -> m [r]
queryLogT_ = queryLog_ . getTypedQuery

queryOnlyLogT :: (QueryParams q, QueryResults r, MonadIOReader m rd, Has Connection rd) => TypedQuery q r -> q -> m (Maybe r)
queryOnlyLogT = queryOnlyLog . getTypedQuery

executeLogT :: (QueryParams q, MonadIOReader m r, Has Connection r) => TypedQuery' q -> q -> m Int64
executeLogT = executeLog . getTypedQuery

executeLogT_ :: (MonadIOReader m r, Has Connection r) => TypedQuery' () -> m Int64
executeLogT_ = executeLog_ . getTypedQuery

executeManyLogT :: (QueryParams q, MonadIOReader m r, Has Connection r) => TypedQuery' q -> [q] -> m Int64
executeManyLogT = executeManyLog . getTypedQuery

class DatabaseTable a where
  type PrimaryKey a
  databaseTableName :: Proxy a -> ByteString
  databaseColumnNames :: Proxy a -> [ByteString]
  databasePrimaryKey :: Proxy a -> [ByteString]

data KeyedRow a = KeyedRow { keyedRowKey :: PrimaryKey a, keyedRowValue :: a }

instance (QueryParams (PrimaryKey a), QueryParams a) => QueryParams (KeyedRow a) where
  renderParams KeyedRow {..} = renderParams (Concat (keyedRowKey, keyedRowValue))
instance (QueryResultsSize (PrimaryKey a), QueryResults a) => QueryResults (KeyedRow a) where
  convertResults fields strings = let
    Concat (keyedRowKey, keyedRowValue) = convertResults fields strings
      in KeyedRow {..}
instance (QueryResultsSize (PrimaryKey a), QueryResultsSize a) => QueryResultsSize (KeyedRow a) where
  queryResultsSize _ = queryResultsSize (Proxy @(PrimaryKey a)) + queryResultsSize (Proxy @a)

queryNameBrackets :: ByteString -> ByteString
queryNameBrackets x = "`" <> x <> "`"

columnListFromNames :: [ByteString] -> ByteString
columnListFromNames = BS.intercalate ", " . map queryNameBrackets

columnUpdateListFromNames :: [ByteString] -> ByteString
columnUpdateListFromNames = BS.intercalate ", " . map (\c -> queryNameBrackets c <> "=VALUES(" <> queryNameBrackets c <> ")")

selectQueryWithSuffix :: forall b a. DatabaseTable a => ByteString -> TypedQuery b a
selectQueryWithSuffix suf = TypedQuery $ Query 
   $ "SELECT " <> columnListFromNames (databaseColumnNames (Proxy @a))
  <> " FROM " <> queryNameBrackets (databaseTableName (Proxy @a)) <> suf

selectQueryKeyedWithSuffix :: forall b a. DatabaseTable a => ByteString -> TypedQuery b (KeyedRow a)
selectQueryKeyedWithSuffix suf = TypedQuery $ Query 
   $ "SELECT " <> columnListFromNames (databasePrimaryKey (Proxy @a) ++ databaseColumnNames (Proxy @a))
  <> " FROM " <> queryNameBrackets (databaseTableName (Proxy @a)) <> suf

selectAllQuery :: DatabaseTable a => TypedQuery () a
selectAllQuery = selectQueryWithSuffix ""

selectAllQueryKeyed :: DatabaseTable a => TypedQuery () (KeyedRow a)
selectAllQueryKeyed = selectQueryKeyedWithSuffix ""

selectByQuery :: DatabaseTable a => ByteString -> TypedQuery (Only b) a
selectByQuery col = selectQueryWithSuffix $ " WHERE " <> queryNameBrackets col <> " = ?"

selectByQueryKeyed :: DatabaseTable a => ByteString -> TypedQuery (Only b) (KeyedRow a)
selectByQueryKeyed col = selectQueryKeyedWithSuffix $ " WHERE " <> queryNameBrackets col <> " = ?"

selectByPrimaryQuery :: forall a. DatabaseTable a => TypedQuery (PrimaryKey a) a
selectByPrimaryQuery = selectQueryWithSuffix $ " WHERE " <> BS.intercalate "AND " (map (<> " = ?") (databasePrimaryKey (Proxy @a)))

insertQuery :: forall a. DatabaseTable a => TypedQuery' a
insertQuery = TypedQuery $ Query 
   $ "INSERT INTO " <> queryNameBrackets (databaseTableName (Proxy @a))
  <> " (" <> columnListFromNames (databaseColumnNames (Proxy @a)) <> ")"
  <> " VALUES (" <> BS.intercalate "," (map (const "?") (databaseColumnNames (Proxy @a))) <> ")"
  <> " ON DUPLICATE KEY UPDATE " <> columnUpdateListFromNames (filter (`notElem` databasePrimaryKey (Proxy @a)) $ databaseColumnNames (Proxy @a))

insertQueryKeyed :: forall a. DatabaseTable a => TypedQuery' (KeyedRow a)
insertQueryKeyed = TypedQuery $ Query 
   $ "INSERT INTO " <> queryNameBrackets (databaseTableName (Proxy @a))
  <> " (" <> columnListFromNames (databasePrimaryKey (Proxy @a) ++ databaseColumnNames (Proxy @a)) <> ")"
  <> " VALUES (?," <> BS.intercalate "," (map (const "?") (databaseColumnNames (Proxy @a))) <> ")"
  <> " ON DUPLICATE KEY UPDATE " <> columnUpdateListFromNames (databaseColumnNames (Proxy @a))
