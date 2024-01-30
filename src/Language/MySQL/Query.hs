{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.MySQL.Query(
  module Language.MySQL.Query, StateT(..), Generic(..), Generically(..)
) where

import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import GHC.Int
import GHC.Word
import Data.Time
import Data.Functor.Identity
import GHC.Generics
import Data.Coerce
import qualified Database.MySQL.Base as Q
import Data.Binary.Put (runPut)

newtype SimpleValue a = SimpleValue { fromSimpleValue :: a }

instance ToMysqlSimple a => ToMysql (SimpleValue a) where
  toMysqlParam = Q.One . toMysqlValue . fromSimpleValue

instance FromMysqlSimple a => FromMysql (SimpleValue a) where
  rowParser = state $ \case
    (x:xs) -> let !v = fromMysqlValue x in (SimpleValue v, xs)
    _ -> undefined -- TODO: better errors

newtype EnumValue a = EnumValue { fromEnumValue :: a }
  deriving (ToMysql, FromMysql) via (SimpleValue (EnumValue a))

class MysqlEnum a where
  fromMysqlEnum :: ST.Text -> a
  toMysqlEnum :: a -> ST.Text

instance MysqlEnum a => ToMysqlSimple (EnumValue a) where
  toMysqlValue (EnumValue a) = Q.MySQLText $ toMysqlEnum a 

instance MysqlEnum a => FromMysqlSimple (EnumValue a) where
  fromMysqlValue (Q.MySQLText x) = EnumValue $ fromMysqlEnum x
  fromMysqlValue v = error $ "Unexpected value: " ++ show v

class ToMysqlSimple a where
  toMysqlValue :: a -> Q.MySQLValue

class ToMysql a where
  toMysqlParam :: a -> Q.Param

toMysql :: ToMysql a => a -> ByteString
toMysql = LB.toStrict . runPut . Q.render . toMysqlParam

instance ToMysqlSimple a => ToMysqlSimple (Maybe a) where
  toMysqlValue (Just a) = toMysqlValue a
  toMysqlValue Nothing = Q.MySQLNull
instance ToMysqlSimple Bool where 
  toMysqlValue True = Q.MySQLInt8 1
  toMysqlValue False = Q.MySQLInt8 0
instance ToMysqlSimple Int8 where toMysqlValue = Q.MySQLInt8
instance ToMysqlSimple Int16 where toMysqlValue = Q.MySQLInt16
instance ToMysqlSimple Int32 where toMysqlValue = Q.MySQLInt32
instance ToMysqlSimple Int where toMysqlValue = Q.MySQLInt64 . fromIntegral
instance ToMysqlSimple Int64 where toMysqlValue = Q.MySQLInt64
instance ToMysqlSimple Integer where toMysqlValue = Q.MySQLInt64 . fromIntegral
instance ToMysqlSimple Word8 where toMysqlValue = Q.MySQLInt8U
instance ToMysqlSimple Word16 where toMysqlValue = Q.MySQLInt16U
instance ToMysqlSimple Word32 where toMysqlValue = Q.MySQLInt32U
instance ToMysqlSimple Word where toMysqlValue = Q.MySQLInt64U . fromIntegral
instance ToMysqlSimple Word64 where toMysqlValue = Q.MySQLInt64U
instance ToMysqlSimple Float where toMysqlValue = Q.MySQLFloat
instance ToMysqlSimple Double where toMysqlValue = Q.MySQLDouble
instance ToMysqlSimple SB.ByteString where toMysqlValue = Q.MySQLBytes
instance ToMysqlSimple LB.ByteString where toMysqlValue = Q.MySQLBytes . LB.toStrict
instance ToMysqlSimple ST.Text where toMysqlValue = Q.MySQLText
instance ToMysqlSimple LT.Text where toMysqlValue = Q.MySQLText . LT.toStrict
instance ToMysqlSimple String where toMysqlValue = Q.MySQLText . ST.pack
instance ToMysqlSimple UTCTime where toMysqlValue = Q.MySQLDateTime . utcToLocalTime utc
instance ToMysqlSimple Day where toMysqlValue = Q.MySQLDate
instance ToMysqlSimple TimeOfDay where toMysqlValue = Q.MySQLTime 0

deriving via (SimpleValue (Maybe a)) instance ToMysqlSimple a => ToMysql (Maybe a)
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

flattenParam :: [Q.Param] -> [Q.MySQLValue]
flattenParam (Q.One x:xs) = x:flattenParam xs
flattenParam (Q.Many x:xs) = x ++ flattenParam xs
flattenParam [] = []

instance (ToMysql a, ToMysql b) => ToMysql (a,b) where
  toMysqlParam (a,b) = Q.Many $ flattenParam [toMysqlParam a, toMysqlParam b]

instance (ToMysql a, ToMysql b, ToMysql c) => ToMysql (a,b,c) where
  toMysqlParam (a,b,c) = Q.Many $ flattenParam [toMysqlParam a, toMysqlParam b, toMysqlParam c]

instance (ToMysql a, ToMysql b, ToMysql c, ToMysql d) => ToMysql (a,b,c,d) where
  toMysqlParam (a,b,c,d) = Q.Many $ flattenParam [toMysqlParam a, toMysqlParam b, toMysqlParam c, toMysqlParam d]

instance (ToMysql a, ToMysql b, ToMysql c, ToMysql d, ToMysql e) => ToMysql (a,b,c,d,e) where
  toMysqlParam (a,b,c,d,e) = Q.Many $ flattenParam [toMysqlParam a, toMysqlParam b, toMysqlParam c, toMysqlParam d, toMysqlParam e]

instance (Generic a, ToMysql (Rep a ())) => ToMysql (Generically a) where
  toMysqlParam (Generically a) = toMysqlParam (from a :: Rep a ())

instance ToMysql c => ToMysql (K1 i c p) where
  toMysqlParam (K1 x) = toMysqlParam x

instance (ToMysql (f p), ToMysql (g p)) => ToMysql ((f :*: g) p) where
  toMysqlParam (f :*: g) = Q.Many $ flattenParam [toMysqlParam f, toMysqlParam g]

instance ToMysql (f p) => ToMysql (M1 i t f p) where
  toMysqlParam (M1 f) = toMysqlParam f

class FromMysqlSimple a where
  fromMysqlValue :: Q.MySQLValue -> a -- TODO: make it not throw

type RowParser = State [Q.MySQLValue]

class FromMysql a where
  rowParser :: RowParser a

instance FromMysqlSimple a => FromMysqlSimple (Maybe a) where
  fromMysqlValue Q.MySQLNull = Nothing
  fromMysqlValue v = Just $ fromMysqlValue v
instance FromMysqlSimple Bool where
  fromMysqlValue (Q.MySQLInt8 x) = x /= 0
  fromMysqlValue v = error $ "Unexpected value: " ++ show v
instance FromMysqlSimple Int8 where
  fromMysqlValue (Q.MySQLInt8 x) = x
  fromMysqlValue v = error $ "Unexpected value: " ++ show v
instance FromMysqlSimple Int16 where
  fromMysqlValue (Q.MySQLInt8 x) = fromIntegral x
  fromMysqlValue (Q.MySQLInt16 x) = x
  fromMysqlValue v = error $ "Unexpected value: " ++ show v
instance FromMysqlSimple Int32 where
  fromMysqlValue (Q.MySQLInt8 x) = fromIntegral x
  fromMysqlValue (Q.MySQLInt16 x) = fromIntegral x
  fromMysqlValue (Q.MySQLInt32 x) = x
  fromMysqlValue v = error $ "Unexpected value: " ++ show v
instance FromMysqlSimple Int where
  fromMysqlValue (Q.MySQLInt8 x) = fromIntegral x
  fromMysqlValue (Q.MySQLInt16 x) = fromIntegral x
  fromMysqlValue (Q.MySQLInt32 x) = fromIntegral x
  fromMysqlValue (Q.MySQLInt64 x) = fromIntegral x
  fromMysqlValue v = error $ "Unexpected value: " ++ show v
instance FromMysqlSimple Int64 where
  fromMysqlValue (Q.MySQLInt8 x) = fromIntegral x
  fromMysqlValue (Q.MySQLInt16 x) = fromIntegral x
  fromMysqlValue (Q.MySQLInt32 x) = fromIntegral x
  fromMysqlValue (Q.MySQLInt64 x) = x
  fromMysqlValue v = error $ "Unexpected value: " ++ show v
instance FromMysqlSimple Integer where
  fromMysqlValue (Q.MySQLInt8 x) = toInteger x
  fromMysqlValue (Q.MySQLInt16 x) = toInteger x
  fromMysqlValue (Q.MySQLInt32 x) = toInteger x
  fromMysqlValue (Q.MySQLInt64 x) = toInteger x
  fromMysqlValue v = error $ "Unexpected value: " ++ show v
instance FromMysqlSimple Word8 where
  fromMysqlValue (Q.MySQLInt8U x) = x
  fromMysqlValue v = error $ "Unexpected value: " ++ show v
instance FromMysqlSimple Word16 where
  fromMysqlValue (Q.MySQLInt8U x) = fromIntegral x
  fromMysqlValue (Q.MySQLInt16U x) = x
  fromMysqlValue v = error $ "Unexpected value: " ++ show v
instance FromMysqlSimple Word32 where
  fromMysqlValue (Q.MySQLInt8U x) = fromIntegral x
  fromMysqlValue (Q.MySQLInt16U x) = fromIntegral x
  fromMysqlValue (Q.MySQLInt32U x) = x
  fromMysqlValue v = error $ "Unexpected value: " ++ show v
instance FromMysqlSimple Word where
  fromMysqlValue (Q.MySQLInt8U x) = fromIntegral x
  fromMysqlValue (Q.MySQLInt16U x) = fromIntegral x
  fromMysqlValue (Q.MySQLInt32U x) = fromIntegral x
  fromMysqlValue (Q.MySQLInt64U x) = fromIntegral x
  fromMysqlValue v = error $ "Unexpected value: " ++ show v
instance FromMysqlSimple Word64 where
  fromMysqlValue (Q.MySQLInt8U x) = fromIntegral x
  fromMysqlValue (Q.MySQLInt16U x) = fromIntegral x
  fromMysqlValue (Q.MySQLInt32U x) = fromIntegral x
  fromMysqlValue (Q.MySQLInt64U x) = x
  fromMysqlValue v = error $ "Unexpected value: " ++ show v
instance FromMysqlSimple Float where
  fromMysqlValue (Q.MySQLFloat x) = x
  fromMysqlValue v = error $ "Unexpected value: " ++ show v
instance FromMysqlSimple Double where
  fromMysqlValue (Q.MySQLDouble x) = x
  fromMysqlValue v = error $ "Unexpected value: " ++ show v
instance FromMysqlSimple Rational where
  fromMysqlValue (Q.MySQLDecimal x) = toRational x
  fromMysqlValue (Q.MySQLFloat x) = toRational x
  fromMysqlValue (Q.MySQLDouble x) = toRational x
  fromMysqlValue v = error $ "Unexpected value: " ++ show v
instance FromMysqlSimple SB.ByteString where
  fromMysqlValue (Q.MySQLBytes x) = x
  fromMysqlValue v = error $ "Unexpected value: " ++ show v
instance FromMysqlSimple LB.ByteString where
  fromMysqlValue (Q.MySQLBytes x) = LB.fromStrict x
  fromMysqlValue v = error $ "Unexpected value: " ++ show v
instance FromMysqlSimple ST.Text where
  fromMysqlValue (Q.MySQLText x) = x
  fromMysqlValue v = error $ "Unexpected value: " ++ show v
instance FromMysqlSimple LT.Text where
  fromMysqlValue (Q.MySQLText x) = LT.fromStrict x
  fromMysqlValue v = error $ "Unexpected value: " ++ show v
instance FromMysqlSimple String where
  fromMysqlValue (Q.MySQLText x) = ST.unpack x
  fromMysqlValue v = error $ "Unexpected value: " ++ show v
instance FromMysqlSimple UTCTime where
  fromMysqlValue (Q.MySQLDateTime x) = localTimeToUTC utc x
  fromMysqlValue v = error $ "Unexpected value: " ++ show v
instance FromMysqlSimple Day where
  fromMysqlValue (Q.MySQLDate x) = x
  fromMysqlValue v = error $ "Unexpected value: " ++ show v
instance FromMysqlSimple TimeOfDay where
  fromMysqlValue (Q.MySQLTime 0 x) = x
  fromMysqlValue v = error $ "Unexpected value: " ++ show v


deriving via (SimpleValue (Maybe a)) instance FromMysqlSimple a => FromMysql (Maybe a)
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

newtype Query o = Query { fromQuery :: ByteString } deriving Show

newtype Command' = Command { fromCommand :: ByteString } deriving Show
type Command = Either ByteString Command'

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