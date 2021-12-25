{-# LANGUAGE OverloadedStrings #-}

module BowBot.API.Hypixel where

import BowBot.API


hypixelWithPlayerData :: APIMonad m => String -> (Object -> Parser a) -> m (Maybe a)
hypixelWithPlayerData uuid f = do
  apiKey <- liftIO $ fromMaybe "" <$> getEnv "HYPIXEL_API"
  let url = "https://api.hypixel.net/player?key=" ++ apiKey ++ "&uuid=" ++ uuid
  let cleanUrl = "https://api.hypixel.net/player?key=[REDACTED]&uuid=" ++ uuid
  res <- hSendRequestTo url cleanUrl
  decodeParse res f

hypixelWithPlayerStatus :: APIMonad m => String -> (Object -> Parser a) -> m (Maybe a)
hypixelWithPlayerStatus uuid f = do
  apiKey <- liftIO $ fromMaybe "" <$> getEnv "HYPIXEL_API"
  let url = "https://api.hypixel.net/status?key=" ++ apiKey ++ "&uuid=" ++ uuid
  let cleanUrl = "https://api.hypixel.net/status?key=[REDACTED]&uuid=" ++ uuid
  res <- hSendRequestTo url cleanUrl
  decodeParse res f
  
hypixelGuildMemberList :: APIMonad m => String -> m (Maybe [String])
hypixelGuildMemberList gid = do
  apiKey <- liftIO $ fromMaybe "" <$> getEnv "HYPIXEL_API"
  let url = "https://api.hypixel.net/guild?key=" ++ apiKey ++ "&id=" ++ gid
  let cleanUrl = "https://api.hypixel.net/guild?key=[REDACTED]&id=" ++ gid
  res <- hSendRequestTo url cleanUrl
  decodeParse res $ \o -> do
    guild <- o .: "guild"
    members <- guild .: "members"
    for members $ \m -> m .: "uuid"