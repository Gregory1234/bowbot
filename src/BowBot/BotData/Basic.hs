{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module BowBot.BotData.Basic where

import Database.MySQL.Simple
import BowBot.DB.Class
import Discord.Internal.Rest (GuildId)
import BowBot.Utils


hInfoDB :: MonadDB m => InfoType a -> m a
hInfoDB InfoType {..} = do
  xs :: [Only String] <- hQueryLog "SELECT `value` FROM `botInfoDEV` WHERE `name` = ?" (Only infoName)
  case xs of
    [Only r] -> case infoParse r of
      Right v -> return v
      Left e -> do
        hLogErrorDB $ "Info perser error in " ++ infoName ++ ": " ++ e
        return infoDefault
    _ -> do
      hLogErrorDB $ "Info not found: " ++ infoName
      return infoDefault

data InfoType a = InfoType { infoName :: String, infoDefault :: a, infoParse :: String -> Either String a }

discordCommandPrefixInfo :: InfoType String
discordCommandPrefixInfo = InfoType { infoName = "command_prefix", infoDefault = "???", infoParse = Right }

readEither :: Read a => String -> Either String a
readEither a = maybe (Left "wrong format") Right $ readMaybe a

discordGuildIdInfo :: InfoType GuildId
discordGuildIdInfo = InfoType { infoName = "discord_guild_id", infoDefault = 0, infoParse = readEither }