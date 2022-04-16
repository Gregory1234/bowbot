{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.Minecraft.Account where

import BowBot.Minecraft.Basic
import BowBot.BotData.Cached
import BowBot.DB.Basic (queryLog, executeManyLog, withDB)
import Data.List.Split (splitOn, chunksOf)
import Data.List (intercalate)
import BowBot.Network.Class
import Data.Proxy (Proxy(..))
import Data.Char (toLower)
import BowBot.Utils
import qualified Data.HashMap.Strict as HM
import BowBot.Network.Monad (runNetworkT)
import Control.Concurrent.Async (mapConcurrently)
import Data.Maybe (catMaybes, mapMaybe)

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
  } deriving (Show, Eq)

instance Cached MinecraftAccount where
  type CacheIndex MinecraftAccount = UUID
  refreshCache conn _ = do
    cache <- getCache (Proxy @MinecraftAccount)
    res :: [(String, String, String)] <- queryLog conn "SELECT `uuid`, `names`, `hypixel` FROM `minecraftDEV`" ()
    let newValues = HM.fromList $ flip fmap res $ \case
          (UUID -> mcUUID, splitOn "," -> mcNames, stringToIsBanned -> Just mcHypixelBow) -> (mcUUID, MinecraftAccount {..})
          (UUID -> mcUUID, splitOn "," -> mcNames, _) -> (mcUUID, MinecraftAccount {mcHypixelBow = NotBanned, ..})
    liftIO $ atomically $ writeTVar cache newValues

instance CachedIndexed MinecraftAccount where
  cacheIndex = mcUUID
  storeInCache accs = do
    cacheMap <- getCacheMap (Proxy @MinecraftAccount)
    let toQueryParams acc@MinecraftAccount {..} = if Just acc == cacheMap HM.!? mcUUID then Nothing else Just (uuidString mcUUID, head mcNames, intercalate "," mcNames, isBannedToString mcHypixelBow)
    let queryParams = mapMaybe toQueryParams accs
    success <- liftIO $ withDB $ \conn -> (>0) <$> executeManyLog conn "INSERT INTO `minecraftDEV` (`uuid`, `name`, `names`, `hypixel`) VALUES (?,?,?,?) ON DUPLICATE KEY UPDATE `name`=VALUES(`name`), `names`=VALUES(`names`), `hypixel`=VALUES(`hypixel`)" queryParams
    when success $ do
      cache <- getCache (Proxy @MinecraftAccount)
      liftIO $ atomically $ modifyTVar cache (insertMany (map (\x -> (mcUUID x, x)) accs))
    return success

instance CachedUpdatable MinecraftAccount where
  updateCache proxy uuids = do
    manager <- hManager
    let helper MinecraftAccount {..} = do
          newNames <- mojangUUIDToNames mcUUID
          return MinecraftAccount {mcNames = fromMaybe mcNames newNames, ..}
    chunked <- fmap (chunksOf 10 . catMaybes) $ for uuids $ getFromCache proxy
    updatedAccounts <- liftIO $ fmap concat $ for chunked $ mapConcurrently (fmap (`runNetworkT` manager) helper)
    void $ storeInCache updatedAccounts

mcNameToUUID :: (MonadCache MinecraftAccount m, MonadNetwork m) => String -> m (Maybe UUID)
mcNameToUUID name = do
  goodAcc <- filter ((==map toLower name) . map toLower . head . mcNames) . HM.elems <$> getCacheMap (Proxy @MinecraftAccount)
  case goodAcc of
    [MinecraftAccount {mcUUID}] -> return (Just mcUUID)
    _ -> mojangNameToUUID name

mcUUIDToNames :: (MonadCache MinecraftAccount m, MonadNetwork m) => UUID -> m (Maybe [String])
mcUUIDToNames uuid = do
  goodAcc <- getFromCache (Proxy @MinecraftAccount) uuid
  case goodAcc of
    Just MinecraftAccount {mcNames} -> return (Just mcNames)
    _ -> mojangUUIDToNames uuid