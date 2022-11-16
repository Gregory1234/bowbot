{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.Perms.Basic where

import BowBot.BotData.Cached
import Discord.Types (UserId)
import BowBot.Discord.DiscordNFData ()
import qualified Data.HashMap.Strict as HM
import BowBot.Utils
import BowBot.DB.Basic (queryLog, withDB, executeManyLog')

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

permissionLevelToString :: PermissionLevel -> String
permissionLevelToString BanLevel = "ban"
permissionLevelToString DefaultLevel = "default"
permissionLevelToString ModLevel = "mod"
permissionLevelToString AdminLevel = "admin"

instance Cached PermissionLevel where
  type CacheIndex PermissionLevel = UserId
  refreshCache = do
    cache <- getCache
    res :: [(Integer, String)] <- queryLog "SELECT `id`, `level` FROM `permissions`" ()
    let newValues = HM.fromList $ flip fmap res $ \case
          (fromInteger -> discord, stringToPermissionLevel -> Just level) -> (discord, level)
          (fromInteger -> discord, _) -> (discord, DefaultLevel)
    liftIO $ atomically $ writeTVar cache newValues

instance CachedStorable PermissionLevel where
  storeInCacheIndexed accs = do
    cacheMap <- getCacheMap
    let toQueryParams (did, lvl) = if Just lvl == cacheMap HM.!? did then Nothing else Just (toInteger did, permissionLevelToString lvl)
    let queryParams = mapMaybe toQueryParams accs
    success <- liftIO $ withDB $ \conn -> (>0) <$> executeManyLog' conn "INSERT INTO `permissions` (`id`, `level`) VALUES (?,?) ON DUPLICATE KEY UPDATE `level`=VALUES(`level`)" queryParams
    when success $ do
      cache <- getCache
      liftIO $ atomically $ modifyTVar cache (insertMany accs)
    return success