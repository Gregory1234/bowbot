{-# LANGUAGE OverloadedStrings #-}

module BowBot.API.Hypixel where

import BowBot.API


hypixelWithPlayerData :: Manager -> String -> (Object -> Parser a) -> IO (Maybe a)
hypixelWithPlayerData manager uuid f = do
  apiKey <- fromMaybe "" <$> getEnv "HYPIXEL_API"
  let url = "https://api.hypixel.net/player?key=" ++ apiKey ++ "&uuid=" ++ uuid
  let cleanUrl = "https://api.hypixel.net/player?key=[REDACTED]&uuid=" ++ uuid
  res <- sendRequestTo manager url cleanUrl
  decodeParse res f

hypixelWithPlayerStatus :: Manager -> String -> (Object -> Parser a) -> IO (Maybe a)
hypixelWithPlayerStatus manager uuid f = do
  apiKey <- fromMaybe "" <$> getEnv "HYPIXEL_API"
  let url = "https://api.hypixel.net/status?key=" ++ apiKey ++ "&uuid=" ++ uuid
  let cleanUrl = "https://api.hypixel.net/status?key=[REDACTED]&uuid=" ++ uuid
  res <- sendRequestTo manager url cleanUrl
  decodeParse res f
  
hypixelGuildMemberList :: Manager -> String -> IO (Maybe [String])
hypixelGuildMemberList man gid = do
  apiKey <- fromMaybe "" <$> getEnv "HYPIXEL_API"
  let url = "https://api.hypixel.net/guild?key=" ++ apiKey ++ "&id=" ++ gid
  let cleanUrl = "https://api.hypixel.net/guild?key=[REDACTED]&id=" ++ gid
  res <- sendRequestTo man url cleanUrl
  decodeParse res $ \o -> do
    guild <- o .: "guild"
    members <- guild .: "members"
    for members $ \m -> m .: "uuid"