{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module BowBot.BotData where

import Control.Concurrent.STM.TVar (TVar, newTVar, writeTVar, modifyTVar, readTVar)
import Data.Map (Map, empty, toList)
import Discord.Types
import BowBot.Stats
import BowBot.API
import BowBot.Utils
import BowBot.Stats.HypixelBow
import Control.Concurrent.STM (STM, atomically)
import Data.Aeson (decode, Value(..))
import Network.HTTP.Conduit (Manager, newManager)
import Data.Traversable (for)
import Data.Text (pack)
import Data.Proxy
import Data.Foldable (traverse_)
import Text.Read (readMaybe)
import Data.Aeson.Types (parseMaybe, (.:), unexpected)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (when)

data ApiRequestCounter = ApiRequestCounter { mainCounter :: TVar Int, borderCounter :: TVar Int, counterLimit :: Int }

clearApiRequestCounter :: ApiRequestCounter -> STM ()
clearApiRequestCounter ApiRequestCounter {..} = do
  border <- readTVar borderCounter
  writeTVar mainCounter border
  writeTVar borderCounter 0

tryApiRequests :: MonadIO m => ApiRequestCounter -> Int -> (Int -> m ()) -> m () -> m ()
tryApiRequests ApiRequestCounter {..} extra onFail onSuccess = do
  t <- liftIO $ read @Int <$> getTime "%S"
  cv <- liftIO . atomically $ do
    c1 <- readTVar mainCounter
    c2 <- readTVar borderCounter
    let c = c1 + c2 + extra
    when (c < counterLimit) $ modifyTVar (if t <= 5 || t >= 55 then borderCounter else mainCounter) (+ extra)
    return $ c < counterLimit
  if cv then onSuccess else onFail ((65 - t) `mod` 60)

data CachedData a = CachedData { mainCache :: TVar (Maybe a), borderCache :: TVar (Maybe a), currentlyBusyCache :: TVar Bool }

data MinecraftAccount = MinecraftAccount
  { mcUUID :: String
  , mcNames :: [String]
  -- TODO: , mcHypixel :: UpdateFreq
  } deriving (Show)

data BowBotAccount = BowBotAccount
  { accountId :: Integer
  , accountDiscords :: [UserId]
  , accountSelectedMinecraft :: String
  , accountMinecrafts :: [String]
  -- TODO: , permission :: PermissionLevel
  } deriving (Show, Eq)

-- TODO: add manager to BotData

data BotData = BotData
  { hypixelRequestCounter :: ApiRequestCounter
  , minecraftAccounts :: TVar [MinecraftAccount]
  , hypixelBowOnlineList :: CachedData [String]
  , hypixelBowSettings :: TVar (Map UserId (Settings HypixelBowStats))
  , bowBotAccounts :: TVar [BowBotAccount]
  }

-- TODO: log errors

downloadMinecraftAccounts :: Manager -> IO (Maybe [MinecraftAccount])
downloadMinecraftAccounts manager = do
  res <- sendDB manager "minecraft/all.php" []
  let parser = parseMaybe $ \o -> do
        dt <- o .: "data"
        for dt $ \acc -> do
          mcUUID <- acc .: "uuid"
          mcNames <- acc .: "names"
          return MinecraftAccount {..}
  return $ decode res >>= parser


downloadBowBotAccounts :: Manager -> IO (Maybe [BowBotAccount])
downloadBowBotAccounts manager = do
  res <- sendDB manager "people/all.php" []
  let parser = parseMaybe $ \o -> do
        dt <- o .: "data"
        for (toList dt) $ \(maybeAccountId, acc) -> do
          accountId <- maybe (unexpected (String (pack maybeAccountId))) return (readMaybe maybeAccountId)
          accountDiscords <- acc .: "discord"
          accountSelectedMinecraft <- acc .: "selected"
          accountMinecrafts <- acc .: "minecraft"
          return BowBotAccount {..}
  return $ decode res >>= parser

downloadData :: BotData -> IO ()
downloadData bdt = do
  manager <- newManager managerSettings
  newMinecraftAccounts <- downloadMinecraftAccounts manager
  newHypixelBowSettings <- getSettings (Proxy @HypixelBowStats) manager
  newBowBotAccounts <- downloadBowBotAccounts manager
  atomically $ do
    traverse_ (writeTVar (minecraftAccounts bdt)) newMinecraftAccounts
    traverse_ (writeTVar (hypixelBowSettings bdt)) newHypixelBowSettings
    traverse_ (writeTVar (bowBotAccounts bdt)) newBowBotAccounts

newRequestCounter :: Int -> STM ApiRequestCounter
newRequestCounter counterLimit = do
  mainCounter <- newTVar 0
  borderCounter <- newTVar 0
  return ApiRequestCounter {..}

newCachedData :: STM (CachedData a)
newCachedData = do
  mainCache <- newTVar Nothing
  borderCache <- newTVar Nothing
  currentlyBusyCache <- newTVar False
  return CachedData {..}

emptyData :: STM BotData
emptyData = do
  hypixelRequestCounter <- newRequestCounter 100
  minecraftAccounts <- newTVar []
  hypixelBowOnlineList <- newCachedData
  hypixelBowSettings <- newTVar empty
  bowBotAccounts <- newTVar []
  return BotData {..}

createData :: IO BotData
createData = do
  bdt <- atomically emptyData
  downloadData bdt
  return bdt