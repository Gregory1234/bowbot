{-# LANGUAGE TypeFamilies #-}

module BowBot.BotData.Info(
  module BowBot.BotData.Info, readEither
) where

import BowBot.DB.Basic
import Discord.Internal.Rest (GuildId)
import BowBot.Utils
import Text.Read (readEither)
import Data.Bifunctor (first)
import qualified Data.Map.Strict as M

data InfoType a = InfoType { infoName :: !Text, infoDefault :: !a, infoParse :: Text -> Either Text a }

newtype InfoCache = InfoCache (TVar (M.Map Text Text))

refreshInfoCache :: (MonadIOReader m r, HasAll '[InfoCache, Connection] r) => m ()
refreshInfoCache = do
  InfoCache cache <- asks getter
  res :: [(Text, Text)] <- queryLog "SELECT `name`, `value` FROM `botInfo`" ()
  liftIO $ atomically $ writeTVar cache (M.fromList res)

downloadInfoCache :: IO InfoCache
downloadInfoCache = do
  cache <- atomically $ newTVar M.empty
  withDB $ \conn -> runReaderT refreshInfoCache (conn, InfoCache cache)
  return (InfoCache cache)

askInfo :: (MonadIOReader m r, Has InfoCache r) => InfoType a -> m a
askInfo InfoType {..} = do
  InfoCache cache <- asks getter
  xs <- liftIO $ atomically $ readTVar cache
  case xs M.!? infoName of
    Just r -> case infoParse r of
      Right v -> return v
      Left e -> do
        logErrorFork $ "Info perser error in " <> infoName <> ": " <> e
        return infoDefault
    _ -> do
      logErrorFork $ "Info not found: " <> infoName
      return infoDefault

discordCommandPrefixInfo :: InfoType Text
discordCommandPrefixInfo = InfoType { infoName = "command_prefix", infoDefault = "???", infoParse = Right }

discordGuildIdInfo :: InfoType GuildId
discordGuildIdInfo = InfoType { infoName = "discord_guild_id", infoDefault = 0, infoParse = first pack . readEither . unpack }

discordStatusInfo :: InfoType Text
discordStatusInfo = InfoType { infoName = "discord_status", infoDefault = "", infoParse = Right }