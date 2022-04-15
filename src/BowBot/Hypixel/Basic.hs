{-# LANGUAGE OverloadedStrings #-}

module BowBot.Hypixel.Basic where

import BowBot.Minecraft.Basic (UUID(..))
import Data.Aeson.Types (Object, Parser, (.:))
import BowBot.Network.Class
import BowBot.Utils
import BowBot.Network.Basic
import BowBot.BotData.Counter

data HypixelApi

instance Counted HypixelApi where
  counterLimit _ = 100

hypixelWithPlayerData :: (MonadNetwork m) => UUID -> (Object -> Parser a) -> m (Maybe a)
hypixelWithPlayerData (UUID uuid) f = do
  apiKey <- liftIO $ fromMaybe "" <$> getEnv "HYPIXEL_API"
  let url = "https://api.hypixel.net/player?key=" ++ apiKey ++ "&uuid=" ++ uuid
  let cleanUrl = "https://api.hypixel.net/player?key=[REDACTED]&uuid=" ++ uuid
  res <- hSendRequestTo url cleanUrl
  decodeParse res f

hypixelWithPlayerStatus :: (MonadNetwork m) => UUID -> (Object -> Parser a) -> m (Maybe a)
hypixelWithPlayerStatus (UUID uuid) f = do
  apiKey <- liftIO $ fromMaybe "" <$> getEnv "HYPIXEL_API"
  let url = "https://api.hypixel.net/status?key=" ++ apiKey ++ "&uuid=" ++ uuid
  let cleanUrl = "https://api.hypixel.net/status?key=[REDACTED]&uuid=" ++ uuid
  res <- hSendRequestTo url cleanUrl
  decodeParse res f
  
hypixelGuildMemberList :: (MonadNetwork m) => String -> m (Maybe [(UUID, String)])
hypixelGuildMemberList gid = do
  apiKey <- liftIO $ fromMaybe "" <$> getEnv "HYPIXEL_API"
  let url = "https://api.hypixel.net/guild?key=" ++ apiKey ++ "&id=" ++ gid
  let cleanUrl = "https://api.hypixel.net/guild?key=[REDACTED]&id=" ++ gid
  res <- hSendRequestTo url cleanUrl
  decodeParse res $ \o -> do
    guild <- o .: "guild"
    members <- guild .: "members"
    for members $ \m -> do
      uuid <- UUID <$> m .: "uuid"
      rank <- m .: "rank"
      return (uuid, rank)