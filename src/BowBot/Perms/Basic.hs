{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.Perms.Basic where

import BowBot.BotData.Cached
import Discord.Types (UserId)
import BowBot.Discord.DiscordNFData ()
import qualified Data.HashMap.Strict as HM
import Data.Proxy
import BowBot.Utils
import BowBot.DB.Basic (queryLog, withDB, executeManyLog)

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
  refreshCache conn _ = do
    cache <- getCache (Proxy @PermissionLevel)
    res :: [(Integer, String)] <- queryLog conn "SELECT `id`, `level` FROM `permissionsDEV`" ()
    let newValues = HM.fromList $ flip fmap res $ \case
          (fromInteger -> discord, stringToPermissionLevel -> Just level) -> (discord, level)
          (fromInteger -> discord, _) -> (discord, DefaultLevel)
    liftIO $ atomically $ writeTVar cache newValues

instance CachedStorable PermissionLevel where
  storeInCacheIndexed accs = do
    let toQueryParams (did, lvl) = (toInteger did, permissionLevelToString lvl)
    success <- liftIO $ withDB $ \conn -> (== fromIntegral (length accs)) <$> executeManyLog conn "INSERT INTO `permissionsDEV` (`id`, `level`) VALUES (?,?) ON DUPLICATE KEY UPDATE `level`=VALUES(`level`)" (map toQueryParams accs)
    when success $ do
      cache <- getCache (Proxy @PermissionLevel)
      liftIO $ atomically $ modifyTVar cache (insertMany accs)
    return success