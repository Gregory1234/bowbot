{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.Hypixel.Basic where

import BowBot.Minecraft.Basic (UUID(..))
import BowBot.Network.Basic
import BowBot.Utils
import BowBot.Counter.Basic

data HypixelApi = HypixelApi

instance Counted HypixelApi where
  counterLimit _ = 100
  counterName _ = "Hypixel"

hypixelWithPlayerData :: (MonadIOReader m r, Has Manager r) => UUID -> (Object -> Parser a) -> m (Maybe a)
hypixelWithPlayerData (UUID uuid) f = do
  apiKey <- liftIO $ maybe "" pack <$> getEnv "HYPIXEL_API"
  let url = "https://api.hypixel.net/player?key=" <> apiKey <> "&uuid=" <> uuid
  let cleanUrl = "https://api.hypixel.net/player?key=[REDACTED]&uuid=" <> uuid
  res <- sendRequestTo url cleanUrl
  decodeParse res f

hypixelWithPlayerStatus :: (MonadIOReader m r, Has Manager r) => UUID -> (Object -> Parser a) -> m (Maybe a)
hypixelWithPlayerStatus (UUID uuid) f = do
  apiKey <- liftIO $ maybe "" pack <$> getEnv "HYPIXEL_API"
  let url = "https://api.hypixel.net/status?key=" <> apiKey <> "&uuid=" <> uuid
  let cleanUrl = "https://api.hypixel.net/status?key=[REDACTED]&uuid=" <> uuid
  res <- sendRequestTo url cleanUrl
  decodeParse res f
  
hypixelGuildMemberList :: (MonadIOReader m r, Has Manager r) => Text -> m (Maybe [(UUID, Text)])
hypixelGuildMemberList gid = do
  apiKey <- liftIO $ maybe "" pack <$> getEnv "HYPIXEL_API"
  let url = "https://api.hypixel.net/guild?key=" <> apiKey <> "&id=" <> gid
  let cleanUrl = "https://api.hypixel.net/guild?key=[REDACTED]&id=" <> gid
  res <- sendRequestTo url cleanUrl
  decodeParse res $ \o -> do
    guild <- o .: "guild"
    members <- guild .: "members"
    for members $ \m -> do
      uuid <- UUID <$> m .: "uuid"
      rank <- m .: "rank"
      return (uuid, rank)