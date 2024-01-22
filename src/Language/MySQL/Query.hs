{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.MySQL.Query(
  module Language.MySQL.Query, Q.Connection, Q.Param(..), Q.Result(..), StateT(..), Generic(..), Generically(..)
) where

import qualified Database.MySQL.Simple.Param as Q
import qualified Database.MySQL.Simple.Result as Q
import qualified Database.MySQL.Simple.QueryParams as Q
import qualified Database.MySQL.Simple.QueryResults as Q
import qualified Database.MySQL.Base.Types as Q
import qualified Database.MySQL.Simple as Q
import qualified Database.MySQL.Simple.Types as Q
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import GHC.Int
import GHC.Word
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeOfDay)
import Data.Functor.Identity
import Data.List (intersperse)
import GHC.Generics
import Data.Coerce

-- TODO: stop depending on mysql-simple

class ToMysql a where
  toActions :: a -> [Q.Action]

toMysql :: ToMysql a => Q.Connection -> a -> IO ByteString
toMysql conn = Q.formatQuery conn "?" . Q.Only . Q.Many . intersperse (Q.Plain ",") . toActions

toMysqlList :: [IO ByteString] -> IO ByteString
toMysqlList xs = SB.intercalate "," <$> sequence xs

deriving via (SimpleValue (Q.In [a])) instance Q.Param a => ToMysql (Q.In [a])

deriving via (SimpleValue (Maybe a)) instance Q.Param a => ToMysql (Maybe a)
deriving via (SimpleValue (Q.Binary SB.ByteString)) instance ToMysql (Q.Binary SB.ByteString)
deriving via (SimpleValue (Q.Binary LB.ByteString)) instance ToMysql (Q.Binary LB.ByteString)
deriving via (SimpleValue Q.Null) instance ToMysql Q.Null
deriving via (SimpleValue Bool) instance ToMysql Bool
deriving via (SimpleValue Int8) instance ToMysql Int8
deriving via (SimpleValue Int16) instance ToMysql Int16
deriving via (SimpleValue Int32) instance ToMysql Int32
deriving via (SimpleValue Int) instance ToMysql Int
deriving via (SimpleValue Int64) instance ToMysql Int64
deriving via (SimpleValue Integer) instance ToMysql Integer
deriving via (SimpleValue Word8) instance ToMysql Word8
deriving via (SimpleValue Word16) instance ToMysql Word16
deriving via (SimpleValue Word32) instance ToMysql Word32
deriving via (SimpleValue Word) instance ToMysql Word
deriving via (SimpleValue Word64) instance ToMysql Word64
deriving via (SimpleValue Float) instance ToMysql Float
deriving via (SimpleValue Double) instance ToMysql Double
deriving via (SimpleValue SB.ByteString) instance ToMysql SB.ByteString
deriving via (SimpleValue LB.ByteString) instance ToMysql LB.ByteString
deriving via (SimpleValue ST.Text) instance ToMysql ST.Text
deriving via (SimpleValue LT.Text) instance ToMysql LT.Text
deriving via (SimpleValue String) instance ToMysql String
deriving via (SimpleValue UTCTime) instance ToMysql UTCTime
deriving via (SimpleValue Day) instance ToMysql Day
deriving via (SimpleValue TimeOfDay) instance ToMysql TimeOfDay

instance (ToMysql a, ToMysql b) => ToMysql (a,b) where
  toActions (a,b) = toActions a ++ toActions b

instance (ToMysql a, ToMysql b, ToMysql c) => ToMysql (a,b,c) where
  toActions (a,b,c) = toActions a ++ toActions b ++ toActions c

instance (ToMysql a, ToMysql b, ToMysql c, ToMysql d) => ToMysql (a,b,c,d) where
  toActions (a,b,c,d) = toActions a ++ toActions b ++ toActions c ++ toActions d

instance (ToMysql a, ToMysql b, ToMysql c, ToMysql d, ToMysql e) => ToMysql (a,b,c,d,e) where
  toActions (a,b,c,d,e) = toActions a ++ toActions b ++ toActions c ++ toActions d ++ toActions e

instance (Generic a, ToMysql (Rep a ())) => ToMysql (Generically a) where
  toActions (Generically a) = toActions (from a :: Rep a ())

instance ToMysql c => ToMysql (K1 i c p) where
  toActions (K1 x) = toActions x

instance (ToMysql (f p), ToMysql (g p)) => ToMysql ((f :*: g) p) where
  toActions (f :*: g) = toActions f ++ toActions g

instance ToMysql (f p) => ToMysql (M1 i t f p) where
  toActions (M1 f) = toActions f

type RowParser = State ([Q.Field], [Maybe ByteString])

textSqlTypes :: [Q.Type]
textSqlTypes = [Q.VarChar,Q.TinyBlob,Q.MediumBlob,Q.LongBlob,Q.Blob,Q.VarString,Q.String,Q.Set,Q.Enum,Q.Json]

class FromMysql a where
  rowParser :: RowParser a

deriving via (SimpleValue (Maybe a)) instance Q.Result a => FromMysql (Maybe a)
deriving via (SimpleValue Bool) instance FromMysql Bool
deriving via (SimpleValue Int8) instance FromMysql Int8
deriving via (SimpleValue Int16) instance FromMysql Int16
deriving via (SimpleValue Int32) instance FromMysql Int32
deriving via (SimpleValue Int) instance FromMysql Int
deriving via (SimpleValue Int64) instance FromMysql Int64
deriving via (SimpleValue Integer) instance FromMysql Integer
deriving via (SimpleValue Word8) instance FromMysql Word8
deriving via (SimpleValue Word16) instance FromMysql Word16
deriving via (SimpleValue Word32) instance FromMysql Word32
deriving via (SimpleValue Word) instance FromMysql Word
deriving via (SimpleValue Word64) instance FromMysql Word64
deriving via (SimpleValue Float) instance FromMysql Float
deriving via (SimpleValue Double) instance FromMysql Double
deriving via (SimpleValue Rational) instance FromMysql Rational
deriving via (SimpleValue SB.ByteString) instance FromMysql SB.ByteString
deriving via (SimpleValue LB.ByteString) instance FromMysql LB.ByteString
deriving via (SimpleValue ST.Text) instance FromMysql ST.Text
deriving via (SimpleValue LT.Text) instance FromMysql LT.Text
deriving via (SimpleValue String) instance FromMysql String
deriving via (SimpleValue UTCTime) instance FromMysql UTCTime
deriving via (SimpleValue Day) instance FromMysql Day
deriving via (SimpleValue TimeOfDay) instance FromMysql TimeOfDay

instance (FromMysql a, FromMysql b) => FromMysql (a,b) where
  rowParser = (,) <$> rowParser <*> rowParser

instance (FromMysql a, FromMysql b, FromMysql c) => FromMysql (a,b,c) where
  rowParser = (,,) <$> rowParser <*> rowParser <*> rowParser

instance (FromMysql a, FromMysql b, FromMysql c, FromMysql d) => FromMysql (a,b,c,d) where
  rowParser = (,,,) <$> rowParser <*> rowParser <*> rowParser <*> rowParser

instance (FromMysql a, FromMysql b, FromMysql c, FromMysql d, FromMysql e) => FromMysql (a,b,c,d,e) where
  rowParser = (,,,,) <$> rowParser <*> rowParser <*> rowParser <*> rowParser <*> rowParser

instance (Generic a, FromMysql (Rep a ())) => FromMysql (Generically a) where
  rowParser = Generically . to @a @() <$> rowParser

instance FromMysql c => FromMysql (K1 i c p) where
  rowParser = coerce (rowParser @c)

instance (FromMysql (f p), FromMysql (g p)) => FromMysql ((f :*: g) p) where
  rowParser = (:*:) <$> rowParser <*> rowParser

instance FromMysql (f p) => FromMysql (M1 i t f p) where
  rowParser = coerce (rowParser @(f p))

newtype SimpleValue a = SimpleValue { fromSimpleValue :: a }

instance Q.Param a => ToMysql (SimpleValue a) where
  toActions = (:[]) . Q.render . fromSimpleValue

instance Q.Result a => FromMysql (SimpleValue a) where
  rowParser = state $ \case
    (f:fs, x:xs) -> let !v = Q.convert f x in (SimpleValue v, (fs, xs))
    _ -> undefined -- TODO: better errors

newtype Flattened a = Flattened { fromFlattened :: a }

instance ToMysql a => Q.QueryParams (Flattened a) where
  renderParams = toActions . fromFlattened

instance FromMysql a => Q.QueryResults (Flattened a) where
  convertResults fs vs = Flattened $ evalState rowParser (fs, vs)

class MysqlString a where
  reqMysqlString :: a -> b -> b
  reqMysqlString _ = id
class MysqlInt a where
  reqMysqlInt :: a -> b -> b
  reqMysqlInt _ = id

reqMysqlString' :: forall a b. MysqlString a => b -> b
reqMysqlString' = reqMysqlString (undefined :: a)
reqMysqlInt' :: forall a b. MysqlInt a => b -> b
reqMysqlInt' = reqMysqlInt (undefined :: a)

instance MysqlString a => MysqlString (Maybe a)
instance MysqlString SB.ByteString
instance MysqlString LB.ByteString
instance MysqlString ST.Text
instance MysqlString LT.Text
instance MysqlString String

instance MysqlInt a => MysqlInt (Maybe a)
instance MysqlInt Bool
instance MysqlInt Int8
instance MysqlInt Int16
instance MysqlInt Int32
instance MysqlInt Int
instance MysqlInt Int64
instance MysqlInt Integer
instance MysqlInt Word8
instance MysqlInt Word16
instance MysqlInt Word32
instance MysqlInt Word
instance MysqlInt Word64

newtype RenderedQuery o = RenderedQuery { fromRenderedQuery :: ByteString } deriving Show

newtype RenderedCommand = RenderedCommand { fromRenderedCommand :: Maybe ByteString } deriving Show

type Query o = Q.Connection -> IO (RenderedQuery o)
type Command = Q.Connection -> IO RenderedCommand

forceQueryType :: r -> Query r -> Query r
forceQueryType _ = id
{-# INLINE forceQueryType #-}

type family TypeNullable t :: Bool where
  TypeNullable (Maybe _) = 'True
  TypeNullable _ = 'False

type family SqlTypeFits exp got :: Bool where
  SqlTypeFits exp exp = 'True
  SqlTypeFits (Maybe exp) exp = 'True
  SqlTypeFits _ _ = 'False -- TODO: make type errors better

type family SqlTypeEqLax t1 t2 :: Bool where
  SqlTypeEqLax (Maybe t1) t2 = SqlTypeEqLax t1 t2
  SqlTypeEqLax t1 (Maybe t2) = SqlTypeEqLax t1 t2
  SqlTypeEqLax t t = 'True
  SqlTypeEqLax _ _ = 'False

reqSqlTypeFits :: SqlTypeFits exp got ~ 'True => exp -> got -> a -> a
reqSqlTypeFits _ _ = id
{-# INLINE reqSqlTypeFits #-}

reqSqlTypeFits1 :: forall got exp a. SqlTypeFits exp got ~ 'True => exp -> a -> a
reqSqlTypeFits1 _ = id
{-# INLINE reqSqlTypeFits1 #-}

reqSqlTypeFits2 :: forall exp got a. SqlTypeFits exp got ~ 'True => got -> a -> a
reqSqlTypeFits2 _ = id
{-# INLINE reqSqlTypeFits2 #-}

reqEqTypeLax :: SqlTypeEqLax t1 t2 ~ 'True => t1 -> t2 -> a -> a
reqEqTypeLax _ _ = id
{-# INLINE reqEqTypeLax #-}

reqEqTypeLax' :: SqlTypeEqLax t1 t2 ~ 'True => t2 -> a -> a
reqEqTypeLax' _ = id
{-# INLINE reqEqTypeLax' #-}

reqNotNullable :: TypeNullable x ~ 'False => x -> a -> a
reqNotNullable _ = id
{-# INLINE reqNotNullable #-}

reqNullable :: Maybe x -> a -> a
reqNullable _ = id
{-# INLINE reqNullable #-}