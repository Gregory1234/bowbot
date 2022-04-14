{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module BowBot.Minecraft.Account where

import BowBot.Minecraft.Basic
import BowBot.DB.Entity
import Data.Maybe (listToMaybe, isJust)
import BowBot.DB.Basic (queryLog, executeManyLog, optionalQueryFilters)
import Database.MySQL.Simple.Types (Only(..))
import Data.List.Split (splitOn)
import Data.List (intercalate)
import BowBot.DB.Class
import BowBot.Network.Class
import Data.Proxy (Proxy(..))

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
  } deriving (Show)

instance DBEntity MinecraftAccount where
  type DBUniqueKey MinecraftAccount = UUID
  data DBFilter MinecraftAccount = MinecraftAccountFilter
    { mcfUUID :: Maybe UUID
    , mcfName :: Maybe String
    , mcfHypixelBow :: Maybe IsBanned
    } deriving (Show)
  emptyFilter = MinecraftAccountFilter { mcfUUID = Nothing, mcfName = Nothing, mcfHypixelBow = Nothing }
  uniqueKey = mcUUID
  getFromDB conn _ mcUUID@(UUID uuid) = do
    res :: [(String, String)] <- queryLog conn "SELECT `names`, `hypixel` FROM `minecraftDEV` WHERE `uuid` = ?" (Only uuid)
    return $ listToMaybe $ flip fmap res $ \case
      (splitOn "," -> mcNames, stringToIsBanned -> Just mcHypixelBow) -> MinecraftAccount {..}
      (splitOn "," -> mcNames, _) -> MinecraftAccount {mcHypixelBow = NotBanned, ..}
  filterFromDB conn MinecraftAccountFilter {..} = do
    res :: [(String, String, String)] <- queryLog conn (optionalQueryFilters [("uuid", isJust mcfUUID), ("name", isJust mcfName), ("hypixel", isJust mcfHypixelBow)] "SELECT `uuid`, `names`, `hypixel` FROM `minecraftDEV`") (uuidString <$> mcfUUID, mcfName, isBannedToString <$> mcfHypixelBow)
    return $ flip fmap res $ \case
      (UUID -> mcUUID, splitOn "," -> mcNames, stringToIsBanned -> Just mcHypixelBow) -> MinecraftAccount {..}
      (UUID -> mcUUID, splitOn "," -> mcNames, _) -> MinecraftAccount {mcHypixelBow = NotBanned, ..}
  storeInDB conn accs = (== fromIntegral (length accs)) <$> executeManyLog conn "INSERT INTO `minecraftDEV` (`uuid`, `name`, `names`, `hypixel`) VALUES (?,?,?,?) ON DUPLICATE KEY UPDATE `name`=VALUES(`name`), `names`=VALUES(`names`), `hypixel`=VALUES(`hypixel`)" (map toQueryParams accs)
      where
        toQueryParams MinecraftAccount {..} = (uuidString mcUUID, head mcNames, intercalate "," mcNames, isBannedToString mcHypixelBow)

mcNameToUUID :: (MonadDB m, MonadNetwork m) => String -> m (Maybe UUID)
mcNameToUUID name = do
  goodAcc <- hFilterFromDB emptyFilter { mcfName = Just name } 
  case goodAcc of
    [MinecraftAccount {mcUUID}] -> return (Just mcUUID)
    _ -> mojangNameToUUID name

mcUUIDToNames :: (MonadDB m, MonadNetwork m) => UUID -> m (Maybe [String])
mcUUIDToNames uuid = do
  goodAcc <- hGetFromDB (Proxy @MinecraftAccount) uuid
  case goodAcc of
    Just MinecraftAccount {mcNames} -> return (Just mcNames)
    _ -> mojangUUIDToNames uuid