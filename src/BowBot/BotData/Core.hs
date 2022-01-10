{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module BowBot.BotData.Core where

import Discord.Types
import Control.Concurrent.STM (STM, TVar)
import Control.Monad.IO.Class
import BowBot.Utils
import Data.Map (Map)
  
data ApiRequestCounter = ApiRequestCounter { mainCounter :: TVar Int, borderCounter :: TVar Int, counterLimit :: Int }

clearApiRequestCounter :: ApiRequestCounter -> STM ()
clearApiRequestCounter ApiRequestCounter {..} = do
  border <- readTVar borderCounter
  writeTVar mainCounter border
  writeTVar borderCounter 0

-- TODO: create a version with a return value
tryApiRequests :: MonadIO m => ApiRequestCounter -> Int -> (Int -> m ()) -> m () -> m ()
tryApiRequests ApiRequestCounter {..} extra onFail onSuccess = do
  t <- liftIO $ read @Int <$> getTime "%S"
  cv <- stm $ do
    c1 <- readTVar mainCounter
    c2 <- readTVar borderCounter
    let c = c1 + c2 + extra
    when (c < counterLimit) $ modifyTVar (if t <= 5 || t >= 55 then borderCounter else mainCounter) (+ extra)
    return $ c < counterLimit
  if cv then onSuccess else onFail ((65 - t) `mod` 60)

tryApiRequestsMulti :: MonadIO m => [(ApiRequestCounter, Int)] -> (Int -> m ()) -> m () -> m ()
tryApiRequestsMulti apis onFail onSuccess = do
  t <- liftIO $ read @Int <$> getTime "%S"
  cv <- stm $ do
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
  writeTVar borderCache Nothing

data CacheResponse a
  = CacheBusy
  | CacheFailed
  | CacheFresh a
  | CacheOld a

getOrCalculateCache :: MonadIO m => CachedData a -> m (Maybe a) -> m (CacheResponse a)
getOrCalculateCache CachedData {..} exec = do
  (ret, busy) <- stm $ do
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
      stm $ do
        writeTVar currentlyBusyCache False
        writeTVar (if t <= 5 || t >= 55 then borderCache else mainCache) val
      return (maybe CacheFailed CacheFresh val)

data UpdateFreq
  = Normal
  | Banned
  deriving (Eq, Ord, Enum, Bounded, Show)

stringToUpdateFreq :: String -> Maybe UpdateFreq
stringToUpdateFreq "normal" = Just Normal
stringToUpdateFreq "ban" = Just Banned
stringToUpdateFreq _ = Nothing

data MinecraftAccount = MinecraftAccount
  { mcUUID :: UUID
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
  , accountSelectedMinecraft :: UUID
  , accountMinecrafts :: [UUID]
  } deriving (Show, Eq)

data SnipeMessage = SnipeMessage
  { snipeMessageAuthor :: UserId
  , snipeMessageContent :: String
  , snipeMessageWasEdited :: Bool
  , snipeMessageTimestamp :: UTCTime
  } deriving Show

data BoolSense = Never | WhenSensible | Always deriving (Show, Eq, Ord, Enum)

data Settings = Settings
  { sWins :: Bool, sLosses :: Bool, sWLR :: BoolSense, sWinsUntil :: BoolSense
  , sBestStreak :: Bool, sCurrentStreak :: Bool, sBestDailyStreak :: Bool
  , sBowHits :: Bool, sBowShots :: Bool, sAccuracy :: BoolSense
  } deriving (Show, Eq)

data BotData = BotData
  { hypixelRequestCounter :: ApiRequestCounter
  , minecraftAccounts :: TVar [MinecraftAccount]
  , hypixelBowOnlineList :: CachedData [UUID]
  , discordPerms :: TVar (Map UserId PermissionLevel)
  , discordSettings :: TVar (Map UserId Settings)
  , bowBotAccounts :: TVar [BowBotAccount]
  , hypixelGuildMembers :: TVar [(UUID, String)]
  , snipeMessage :: TVar (Map ChannelId SnipeMessage)
  , hypixelGuildId :: TVar String
  , discordGuildId :: TVar GuildId
  , discordIllegalRole :: TVar RoleId
  , discordMemberRole :: TVar RoleId
  , discordVisitorRole :: TVar RoleId
  , discordDivisionRoles :: TVar [(Integer, RoleId)]
  , discordToggleableRoles :: TVar [(String, RoleId)]
  , discordOtherSavedRoles :: TVar [(String, RoleId)]
  , discordHypixelRoles :: TVar [(String, [String], RoleId)]
  , discordCommandPrefix :: TVar String
  , discordBirthdayChannel :: TVar ChannelId
  }