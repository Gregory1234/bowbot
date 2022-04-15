{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.BotData.Info where

import BowBot.DB.Basic
import Discord.Internal.Rest (GuildId)
import BowBot.Utils
import qualified Data.HashMap.Strict as HM
import BowBot.BotData.Cached
import Data.Proxy

data InfoField = InfoField { infoFieldName :: String, infoFieldValue :: String }

instance Cached InfoField where
  type CacheIndex InfoField = String
  refreshCache conn _ = do
    cache <- getCache (Proxy @InfoField)
    res :: [(String, String)] <- queryLog conn "SELECT `name`, `value` FROM `botInfoDEV`" ()
    let newValues = HM.fromList $ flip fmap res $ \(infoFieldName, infoFieldValue) -> (infoFieldName, InfoField {..})
    liftIO $ atomically $ writeTVar cache newValues

instance CachedIndexed InfoField where
  cacheIndex = infoFieldName
  storeInCache accs = do
    let toQueryParams InfoField {..} = (infoFieldName, infoFieldValue)
    success <- liftIO $ withDB $ \conn -> (== fromIntegral (length accs)) <$> executeManyLog conn "INSERT INTO `botInfoDEV` (`name`, `value`) VALUES (?,?) ON DUPLICATE KEY UPDATE `value`=VALUES(`value`)" (map toQueryParams accs)
    when success $ do
      cache <- getCache (Proxy @InfoField)
      liftIO $ atomically $ modifyTVar cache (insertMany $ map (\x -> (infoFieldValue x, x)) accs)
    return success

hInfoDB :: MonadCache InfoField m => InfoType a -> m a
hInfoDB InfoType {..} = do
  xs <- getFromCache (Proxy @InfoField) infoName
  case xs of
    Just InfoField { infoFieldValue = r } -> case infoParse r of
      Right v -> return v
      Left e -> do
        logError $ "Info perser error in " ++ infoName ++ ": " ++ e
        return infoDefault
    _ -> do
      logError $ "Info not found: " ++ infoName
      return infoDefault

data InfoType a = InfoType { infoName :: String, infoDefault :: a, infoParse :: String -> Either String a }

discordCommandPrefixInfo :: InfoType String
discordCommandPrefixInfo = InfoType { infoName = "command_prefix", infoDefault = "???", infoParse = Right }

readEither :: Read a => String -> Either String a
readEither a = maybe (Left "wrong format") Right $ readMaybe a

discordGuildIdInfo :: InfoType GuildId
discordGuildIdInfo = InfoType { infoName = "discord_guild_id", infoDefault = 0, infoParse = readEither }