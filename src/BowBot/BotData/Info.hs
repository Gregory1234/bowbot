{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.BotData.Info(
  module BowBot.BotData.Info, readEither
) where

import BowBot.DB.Basic
import Discord.Internal.Rest (GuildId)
import BowBot.Utils
import qualified Data.HashMap.Strict as HM
import BowBot.BotData.Cached
import Text.Read (readEither)

data InfoField = InfoField { infoFieldName :: String, infoFieldValue :: String } deriving (Show, Eq)

instance Cached InfoField where
  type CacheIndex InfoField = String
  refreshCache = do
    cache <- getCache
    res :: [(String, String)] <- queryLog "SELECT `name`, `value` FROM `botInfo`" ()
    let newValues = HM.fromList $ flip fmap res $ \(infoFieldName, infoFieldValue) -> (infoFieldName, InfoField {..})
    liftIO $ atomically $ writeTVar cache newValues

instance CachedIndexed InfoField where
  cacheIndex = infoFieldName
  storeInCache accs = do
    cacheMap <- getCacheMap
    let toQueryParams f@InfoField {..} = if Just f == cacheMap HM.!? infoFieldName then Nothing else Just (infoFieldName, infoFieldValue)
    let queryParams = mapMaybe toQueryParams accs
    success <- liftIO $ withDB $ \conn -> (>0) <$> executeManyLog' conn "INSERT INTO `botInfo` (`name`, `value`) VALUES (?,?) ON DUPLICATE KEY UPDATE `value`=VALUES(`value`)" queryParams
    when success $ do
      cache <- getCache
      liftIO $ atomically $ modifyTVar cache (insertMany $ map (\x -> (infoFieldValue x, x)) accs)
    return success

askInfo :: (MonadIOBotData m d r, HasCache InfoField d) => InfoType a -> m a
askInfo InfoType {..} = do
  xs <- getFromCache infoName
  case xs of
    Just InfoField { infoFieldValue = r } -> case infoParse r of
      Right v -> return v
      Left e -> do
        logErrorFork $ "Info perser error in " ++ infoName ++ ": " ++ e
        return infoDefault
    _ -> do
      logErrorFork $ "Info not found: " ++ infoName
      return infoDefault

data InfoType a = InfoType { infoName :: String, infoDefault :: a, infoParse :: String -> Either String a }

discordCommandPrefixInfo :: InfoType String
discordCommandPrefixInfo = InfoType { infoName = "command_prefix", infoDefault = "???", infoParse = Right }

discordGuildIdInfo :: InfoType GuildId
discordGuildIdInfo = InfoType { infoName = "discord_guild_id", infoDefault = 0, infoParse = readEither }

discordStatusInfo :: InfoType String
discordStatusInfo = InfoType { infoName = "discord_status", infoDefault = "", infoParse = Right }