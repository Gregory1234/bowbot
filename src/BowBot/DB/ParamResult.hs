module BowBot.DB.ParamResult(
  module BowBot.DB.ParamResult, Q.Param(render), Q.Result
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
import Control.Monad.Writer.Strict
import Control.Monad.Except
import Data.ByteString (ByteString)
import Data.Set (Set)
import GHC.Int
import GHC.Word
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeOfDay)
import Data.Functor.Identity
import Data.Coerce

newtype Flattened a = Flattened { fromFlattened :: a }

class QueryParams a where
  renderParams :: a -> [Q.Action]

instance QueryParams a => Q.QueryParams (Flattened a) where
  renderParams = renderParams . fromFlattened

type ResultConverterState = [(Q.Field, Maybe ByteString)]

newtype ResultConverter a = ResultConverter { runResultConverter :: ResultConverterState -> (Either () (a, ResultConverterState), Sum Int) }
  deriving (Functor, Applicative, Monad, MonadState ResultConverterState, MonadError (), MonadWriter (Sum Int)) via (StateT ResultConverterState (ExceptT () (Writer (Sum Int))))

class QueryResults a where
  convertResults :: ResultConverter a

instance QueryResults a => Q.QueryResults (Flattened a) where
  convertResults fs vs = case runResultConverter convertResults (zip fs vs) of
    (Right (a, []), _) -> Flattened a
    (_, s) -> Q.convertError fs vs (getSum s)

newtype SimpleValue a = SimpleValue { fromSimpleValue :: a }

instance Q.Param a => QueryParams (SimpleValue a) where
  renderParams (SimpleValue a) = [Q.render a]

convert :: Q.Result a => ResultConverter a
convert = ResultConverter $ \s -> (,1) $ case s of
  [] -> Left ()
  (f,v):fvs -> let !val = Q.convert f v in Right (val, fvs)

instance Q.Result a => QueryResults (SimpleValue a) where
  convertResults = coerce (convert @a)


deriving via (SimpleValue Q.Action) instance QueryParams Q.Action
deriving via (SimpleValue (Maybe a)) instance Q.Param a => QueryParams (Maybe a)
deriving via (SimpleValue (Q.In [a])) instance Q.Param a => QueryParams (Q.In [a])
deriving via (SimpleValue (Q.In (Set a))) instance Q.Param a => QueryParams (Q.In (Set a))
deriving via (SimpleValue (Q.VaArgs [a])) instance Q.Param a => QueryParams (Q.VaArgs [a])
deriving via (SimpleValue (Q.Binary SB.ByteString)) instance QueryParams (Q.Binary SB.ByteString)
deriving via (SimpleValue (Q.Binary LB.ByteString)) instance QueryParams (Q.Binary LB.ByteString)
deriving via (SimpleValue Q.Null) instance QueryParams Q.Null
deriving via (SimpleValue Bool) instance QueryParams Bool
deriving via (SimpleValue Int8) instance QueryParams Int8
deriving via (SimpleValue Int16) instance QueryParams Int16
deriving via (SimpleValue Int32) instance QueryParams Int32
deriving via (SimpleValue Int) instance QueryParams Int
deriving via (SimpleValue Int64) instance QueryParams Int64
deriving via (SimpleValue Integer) instance QueryParams Integer
deriving via (SimpleValue Word8) instance QueryParams Word8
deriving via (SimpleValue Word16) instance QueryParams Word16
deriving via (SimpleValue Word32) instance QueryParams Word32
deriving via (SimpleValue Word) instance QueryParams Word
deriving via (SimpleValue Word64) instance QueryParams Word64
deriving via (SimpleValue Float) instance QueryParams Float
deriving via (SimpleValue Double) instance QueryParams Double
deriving via (SimpleValue SB.ByteString) instance QueryParams SB.ByteString
deriving via (SimpleValue LB.ByteString) instance QueryParams LB.ByteString
deriving via (SimpleValue ST.Text) instance QueryParams ST.Text
deriving via (SimpleValue LT.Text) instance QueryParams LT.Text
deriving via (SimpleValue String) instance QueryParams String
deriving via (SimpleValue UTCTime) instance QueryParams UTCTime
deriving via (SimpleValue Day) instance QueryParams Day
deriving via (SimpleValue TimeOfDay) instance QueryParams TimeOfDay

deriving via (SimpleValue (Maybe a)) instance Q.Result a => QueryResults (Maybe a)
deriving via (SimpleValue Bool) instance QueryResults Bool
deriving via (SimpleValue Int8) instance QueryResults Int8
deriving via (SimpleValue Int16) instance QueryResults Int16
deriving via (SimpleValue Int32) instance QueryResults Int32
deriving via (SimpleValue Int) instance QueryResults Int
deriving via (SimpleValue Int64) instance QueryResults Int64
deriving via (SimpleValue Integer) instance QueryResults Integer
deriving via (SimpleValue Word8) instance QueryResults Word8
deriving via (SimpleValue Word16) instance QueryResults Word16
deriving via (SimpleValue Word32) instance QueryResults Word32
deriving via (SimpleValue Word) instance QueryResults Word
deriving via (SimpleValue Word64) instance QueryResults Word64
deriving via (SimpleValue Float) instance QueryResults Float
deriving via (SimpleValue Double) instance QueryResults Double
deriving via (SimpleValue Rational) instance QueryResults Rational
deriving via (SimpleValue SB.ByteString) instance QueryResults SB.ByteString
deriving via (SimpleValue LB.ByteString) instance QueryResults LB.ByteString
deriving via (SimpleValue ST.Text) instance QueryResults ST.Text
deriving via (SimpleValue LT.Text) instance QueryResults LT.Text
deriving via (SimpleValue String) instance QueryResults String
deriving via (SimpleValue UTCTime) instance QueryResults UTCTime
deriving via (SimpleValue Day) instance QueryResults Day
deriving via (SimpleValue TimeOfDay) instance QueryResults TimeOfDay

instance QueryParams () where
  renderParams () = []
instance (QueryParams a, QueryParams b) => QueryParams (a,b) where
  renderParams (a,b) = renderParams a ++ renderParams b
instance (QueryParams a, QueryParams b, QueryParams c) => QueryParams (a,b,c) where
  renderParams (a,b,c) = renderParams a ++ renderParams b ++ renderParams c
instance (QueryParams a, QueryParams b, QueryParams c, QueryParams d) => QueryParams (a,b,c,d) where
  renderParams (a,b,c,d) = renderParams a ++ renderParams b ++ renderParams c ++ renderParams d

instance (QueryResults a, QueryResults b) => QueryResults (a,b) where
  convertResults = (,) <$> convertResults <*> convertResults
instance (QueryResults a, QueryResults b, QueryResults c) => QueryResults (a,b,c) where
  convertResults = (,,) <$> convertResults <*> convertResults <*> convertResults
instance (QueryResults a, QueryResults b, QueryResults c, QueryResults d) => QueryResults (a,b,c,d) where
  convertResults = (,,,) <$> convertResults <*> convertResults <*> convertResults <*> convertResults
