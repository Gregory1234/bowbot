{-# LANGUAGE DerivingVia #-}

module BowBot.CommandMonads where

import Network.HTTP.Conduit hiding (path)
import BowBot.Utils
import Control.Monad.Reader (ReaderT(..), MonadTrans(..))
import Database.MySQL.Simple (Connection)
import BowBot.BotData.Core
import Discord (DiscordHandler)
import Control.Concurrent.STM.TVar (TVar)

class MonadIO m => MonadHoistIO m where
  hoistIO :: (IO a -> IO b) -> m a -> m b

instance MonadHoistIO IO where
  hoistIO = id

instance MonadHoistIO m => MonadHoistIO (ReaderT r m) where
  hoistIO f (ReaderT a) = ReaderT $ hoistIO f . a

class MonadIO m => APIMonad m where
  hManager :: m Manager

newtype ManagerT m a = ManagerT { runManagerT :: Manager -> m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadHoistIO, DBMonad, BotDataMonad, DiscordMonad) via (ReaderT Manager m)
  deriving (MonadTrans) via (ReaderT Manager)

instance MonadIO m => APIMonad (ManagerT m) where
  hManager = ManagerT return

instance APIMonad m => APIMonad (ReaderT r m) where
  hManager = lift hManager

class MonadIO m => DBMonad m where
  hConnection :: m Connection

newtype ConnectionT m a = ConnectionT { runConnectionT :: Connection -> m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadHoistIO, APIMonad, BotDataMonad, DiscordMonad) via (ReaderT Connection m)
  deriving (MonadTrans) via (ReaderT Connection)

instance MonadIO m => DBMonad (ConnectionT m) where
  hConnection = ConnectionT return

instance DBMonad m => DBMonad (ReaderT r m) where
  hConnection = lift hConnection

class MonadIO m => BotDataMonad m where
  hData :: m BotData

newtype BotDataT m a = BotDataT { runBotDataT :: BotData -> m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadHoistIO, APIMonad, DBMonad, DiscordMonad) via (ReaderT BotData m)
  deriving (MonadTrans) via (ReaderT BotData)

instance MonadIO m => BotDataMonad (BotDataT m) where
  hData = BotDataT return

instance BotDataMonad m => BotDataMonad (ReaderT r m) where
  hData = lift hData

hRead :: BotDataMonad m => (BotData -> TVar a) -> m a
hRead p = hData >>= readProp p

hModify :: BotDataMonad m => (BotData -> TVar a) -> (a -> a) -> m ()
hModify p f = hData >>= flip (modifyProp p) f

hWrite :: BotDataMonad m => (BotData -> TVar a) -> a -> m ()
hWrite p v = hData >>= flip (writeProp p) v

hTryApiRequests :: BotDataMonad m => (BotData -> ApiRequestCounter) -> Int -> (Int -> m ()) -> m () -> m ()
hTryApiRequests counter extra onFail onSuccess = do
  dt <- hData
  tryApiRequests (counter dt) extra onFail onSuccess

hTryApiRequestsMulti :: BotDataMonad m => [(BotData -> ApiRequestCounter, Int)] -> (Int -> m ()) -> m () -> m ()
hTryApiRequestsMulti apis onFail onSuccess = do
  dt <- hData
  tryApiRequestsMulti (map (\(a,b) -> (a dt, b)) apis) onFail onSuccess
  
hCache :: BotDataMonad m => (BotData -> CachedData a) -> m (Maybe a) -> m (CacheResponse a)
hCache cache exec = do
  dt <- hData
  getOrCalculateCache (cache dt) exec

class MonadIO m => DiscordMonad m where
  hDiscord :: DiscordHandler a -> m a

newtype DiscordHandler' a = DiscordHandler' { runDiscordHandler' :: DiscordHandler a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadHoistIO) via DiscordHandler

instance DiscordMonad DiscordHandler' where
  hDiscord = DiscordHandler'

instance DiscordMonad m => DiscordMonad (ReaderT r m) where
  hDiscord = ReaderT . const . hDiscord