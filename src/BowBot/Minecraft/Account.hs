{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.Minecraft.Account where

import BowBot.Minecraft.Basic
import BowBot.BotData.Cached
import BowBot.DB.Basic (queryLog, executeManyLog, withDB, logInfo)
import BowBot.Network.Basic
import BowBot.Utils
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent (threadDelay)

data IsBanned
  = NotBanned
  | Banned
  deriving (Eq, Ord, Enum, Bounded, Show)

stringToIsBanned :: String -> Maybe IsBanned
stringToIsBanned "normal" = Just NotBanned
stringToIsBanned "ban" = Just Banned
stringToIsBanned _ = Nothing

isBannedToString :: IsBanned -> String
isBannedToString NotBanned = "normal"
isBannedToString Banned = "ban"

data MinecraftAccount = MinecraftAccount
  { mcUUID :: UUID
  , mcNames :: [String]
  , mcHypixelBow :: IsBanned
  , mcHypixelWatchlist :: Bool
  } deriving (Show, Eq)

instance Cached MinecraftAccount where
  type CacheIndex MinecraftAccount = UUID
  refreshCache conn = do
    cache <- getCache
    res :: [(String, String, String, Bool)] <- queryLog conn "SELECT `uuid`, `names`, `hypixel`, `watchlist` FROM `minecraftDEV`" ()
    let newValues = HM.fromList $ flip fmap res $ \case
          (UUID -> mcUUID, splitOn "," -> mcNames, stringToIsBanned -> Just mcHypixelBow, mcHypixelWatchlist) -> (mcUUID, MinecraftAccount {..})
          (UUID -> mcUUID, splitOn "," -> mcNames, _, mcHypixelWatchlist) -> (mcUUID, MinecraftAccount {mcHypixelBow = NotBanned, ..})
    liftIO $ atomically $ writeTVar cache newValues

instance CachedIndexed MinecraftAccount where
  cacheIndex = mcUUID
  storeInCache accs = do
    cacheMap <- getCacheMap
    let toQueryParams acc@MinecraftAccount {..} = if Just acc == cacheMap HM.!? mcUUID then Nothing else Just (uuidString mcUUID, head mcNames, intercalate "," mcNames, isBannedToString mcHypixelBow, if mcHypixelWatchlist then 1 :: Integer else 0)
    let queryParams = mapMaybe toQueryParams accs
    success <- liftIO $ withDB $ \conn -> (>0) <$> executeManyLog conn "INSERT INTO `minecraftDEV` (`uuid`, `name`, `names`, `hypixel`, `watchlist`) VALUES (?,?,?,?,?) ON DUPLICATE KEY UPDATE `name`=VALUES(`name`), `names`=VALUES(`names`), `hypixel`=VALUES(`hypixel`), `watchlist`=VALUES(`watchlist`)" queryParams
    when success $ do
      cache <- getCache
      liftIO $ atomically $ modifyTVar cache (insertMany (map (\x -> (mcUUID x, x)) accs))
    return success

updateMinecraftAccountCache :: (MonadIO m, MonadReader r m, HasBotData d r, Has Manager r, HasCache MinecraftAccount d) => m ()
updateMinecraftAccountCache = do
  ctx <- ask
  let helper MinecraftAccount {..} = do
        newNames <- mojangUUIDToNames mcUUID
        return MinecraftAccount {mcNames = fromMaybe mcNames newNames, ..}
  cache <- HM.elems <$> getCacheMap
  let bigchunked = chunksOf 400 cache
  updatedAccounts <- liftIO $ fmap concat $ sequence $ intersperse (([] <$) $ logInfo "Started 10 minute wait in Minecraft update" >> threadDelay 600000000) $ flip map bigchunked $ \bigchunk -> do
    let chunked = chunksOf 10 bigchunk
    fmap concat $ for chunked $ mapConcurrently (fmap (`runReaderT` ctx) helper)
  void $ storeInCache updatedAccounts

mcNameToUUID :: (MonadIO m, MonadReader r m, HasBotData d r, Has Manager r, HasCache MinecraftAccount d) => String -> m (Maybe UUID)
mcNameToUUID name = do
  goodAcc <- getMinecraftAccountByCurrentNameFromCache name
  case goodAcc of
    Just MinecraftAccount {mcUUID} -> return (Just mcUUID)
    _ -> mojangNameToUUID name

mcUUIDToNames :: (MonadIO m, MonadReader r m, HasBotData d r, Has Manager r, HasCache MinecraftAccount d) => UUID -> m (Maybe [String])
mcUUIDToNames uuid = do
  goodAcc <- getFromCache uuid
  case goodAcc of
    Just MinecraftAccount {mcNames} -> return (Just mcNames)
    _ -> mojangUUIDToNames uuid

getMinecraftAccountByCurrentNameFromCache :: (MonadIO m, MonadReader r m, HasBotData d r, HasCache MinecraftAccount d) => String -> m (Maybe MinecraftAccount)
getMinecraftAccountByCurrentNameFromCache name = find ((==map toLower name) . map toLower . head . mcNames) . HM.elems <$> getCacheMap
  