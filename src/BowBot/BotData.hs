{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module BowBot.BotData where

import Control.Concurrent.STM.TVar (TVar, newTVar)
import Data.Map (Map, empty, toList, fromList)
import Discord.Types
import BowBot.API
import BowBot.Settings
import Control.Concurrent.STM (STM)
import Network.HTTP.Conduit (newManager)
import Data.List.Split (chunksOf)
import Control.Concurrent.Async (mapConcurrently)
import BowBot.API.Mojang (mojangUUIDToNames)
import Data.List (intercalate)

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

tryApiRequestsMulti :: MonadIO m => [(ApiRequestCounter, Int)] -> (Int -> m ()) -> m () -> m ()
tryApiRequestsMulti apis onFail onSuccess = do
  t <- liftIO $ read @Int <$> getTime "%S"
  cv <- liftIO . atomically $ do
    res <- fmap and . for apis $ \(ApiRequestCounter {..}, extra) -> do
      c1 <- readTVar mainCounter
      c2 <- readTVar borderCounter
      let c = c1 + c2 + extra
      return $ c < counterLimit
    when res $ for_ apis $ \(ApiRequestCounter {..}, extra) -> do
      modifyTVar (if t <= 5 || t >= 55 then borderCounter else mainCounter) (+ extra)
    return res
  if cv then onSuccess else onFail ((65 - t) `mod` 60)

data CachedData a = CachedData { mainCache :: TVar (Maybe a), borderCache :: TVar (Maybe a), currentlyBusyCache :: TVar Bool }

clearCache :: CachedData a -> STM ()
clearCache CachedData {..} = do
  border <- readTVar borderCache
  writeTVar mainCache border

data CacheResponse a
  = CacheBusy
  | CacheFailed
  | CacheFresh a
  | CacheOld a

getOrCalculateCache :: MonadIO m => CachedData a -> m (Maybe a) -> m (CacheResponse a)
getOrCalculateCache CachedData {..} exec = do
  (ret, busy) <- liftIO $ atomically $ do
    busy <- readTVar currentlyBusyCache
    mainVal <- readTVar mainCache
    borderVal <- readTVar borderCache
    case (mainVal, borderVal) of
      (Nothing, Nothing) -> do
        unless busy $ writeTVar currentlyBusyCache True
        return (Nothing, busy)
      (Nothing, _) -> return (borderVal, busy)
      (Just _,_) -> return (mainVal, busy)
  case (ret, busy) of
    (Nothing, True) -> return CacheBusy
    (Just a, _) -> return (CacheOld a)
    (Nothing, False) -> do
      val <- exec
      t <- liftIO $ read @Int <$> getTime "%S"
      liftIO $ atomically $ do
        writeTVar currentlyBusyCache False
        writeTVar (if t <= 5 || t >= 55 then borderCache else mainCache) val
      return (maybe CacheFailed CacheFresh val)

data UpdateFreq
  = BiHourly
  | Daily
  | Weekly
  | Banned
  deriving (Eq, Ord, Enum, Bounded, Show)

stringToUpdateFreq :: String -> Maybe UpdateFreq
stringToUpdateFreq "bihour" = Just BiHourly
stringToUpdateFreq "day" = Just Daily
stringToUpdateFreq "week" = Just Weekly
stringToUpdateFreq "ban" = Just Banned
stringToUpdateFreq _ = Nothing

data MinecraftAccount = MinecraftAccount
  { mcUUID :: String
  , mcNames :: [String]
  , mcHypixelBow :: UpdateFreq
  } deriving (Show)

data PermissionLevel
  = BanLevel
  | DefaultLevel
  | ModLevel
  | AdminLevel
  deriving (Eq, Ord, Enum, Bounded, Show)

stringToPermissionLevel :: String -> Maybe PermissionLevel
stringToPermissionLevel "ban" = Just BanLevel
stringToPermissionLevel "default" = Just DefaultLevel
stringToPermissionLevel "mod" = Just ModLevel
stringToPermissionLevel "admin" = Just AdminLevel
stringToPermissionLevel _ = Nothing

data BowBotAccount = BowBotAccount
  { accountId :: Integer
  , accountDiscords :: [UserId]
  , accountSelectedMinecraft :: String
  , accountMinecrafts :: [String]
  } deriving (Show, Eq)

-- TODO: add manager to BotData

data BotData = BotData
  { hypixelRequestCounter :: ApiRequestCounter
  , minecraftAccounts :: TVar [MinecraftAccount]
  , hypixelBowOnlineList :: CachedData [String]
  , discordPerms :: TVar (Map UserId PermissionLevel)
  , discordSettings :: TVar (Map UserId Settings)
  , bowBotAccounts :: TVar [BowBotAccount]
  , hypixelGuildMembers :: TVar (Maybe [String])
  , snipeMessage :: TVar (Map ChannelId (UserId, String))
  }

-- TODO: log errors

downloadMinecraftAccounts :: Manager -> IO (Maybe [MinecraftAccount])
downloadMinecraftAccounts manager = do
  res <- sendDB manager "minecraft/all.php" []
  decodeParse res $ \o -> do
    dt <- o .: "data"
    for dt $ \acc -> do
      mcUUID <- acc .: "uuid"
      mcNames <- acc .: "names"
      (stringToUpdateFreq -> Just mcHypixelBow) <- acc .: "hypixel"
      return MinecraftAccount {..}


downloadBowBotAccounts :: Manager -> IO (Maybe [BowBotAccount])
downloadBowBotAccounts manager = do
  res <- sendDB manager "people/all.php" []
  decodeParse res $ \o -> do
    dt <- o .: "data"
    for (toList dt) $ \(accountId, acc) -> do
      accountDiscords <- acc .: "discord"
      accountSelectedMinecraft <- acc .: "selected"
      accountMinecrafts <- acc .: "minecraft"
      return BowBotAccount {..}

downloadDiscordPerms :: Manager -> IO (Maybe (Map UserId PermissionLevel))
downloadDiscordPerms manager = do
  res <- sendDB manager "discord/perms.php" []
  decodeParse res $ \o -> do
    dt <- o .: "data"
    fmap fromList . for dt $ \dp -> do
      did <- dp .: "id"
      (stringToPermissionLevel -> Just level) <- dp .: "level"
      return (did, level)

downloadData :: BotData -> IO ()
downloadData bdt = do
  manager <- newManager managerSettings
  newMinecraftAccounts <- downloadMinecraftAccounts manager
  newDiscordSettings <- getSettings manager
  newBowBotAccounts <- downloadBowBotAccounts manager
  newDiscordPerms <- downloadDiscordPerms manager
  atomically $ do
    for_ newMinecraftAccounts (writeTVar (minecraftAccounts bdt))
    for_ newDiscordSettings (writeTVar (discordSettings bdt))
    for_ newBowBotAccounts (writeTVar (bowBotAccounts bdt))
    for_ newDiscordPerms (writeTVar (discordPerms bdt))

updateMinecraftAccounts :: BotData -> Manager -> IO ()
updateMinecraftAccounts bdt manager = do
  nickList <- atomically $ readTVar $ minecraftAccounts bdt
  let chunked = chunksOf 10 nickList
  updatedNicks <- fmap concat $ for chunked $ mapConcurrently helper
  atomically $ writeTVar (minecraftAccounts bdt) updatedNicks
  where
    helper MinecraftAccount {..} = do
      newNames <- mojangUUIDToNames manager mcUUID
      for_ newNames $ \names ->
        unless (mcNames == names) $ updateMinecraftNames mcUUID names
      return MinecraftAccount {mcNames = fromMaybe mcNames newNames, ..}
    updateMinecraftNames uuid names = 
      void $ sendDB manager "minecraft/setnames.php" ["uuid=" ++ uuid, "names=" ++ intercalate "," names]


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
  discordSettings <- newTVar empty
  bowBotAccounts <- newTVar []
  discordPerms <- newTVar empty
  hypixelGuildMembers <- newTVar Nothing
  snipeMessage <- newTVar empty
  return BotData {..}

createData :: IO BotData
createData = do
  bdt <- atomically emptyData
  downloadData bdt
  return bdt