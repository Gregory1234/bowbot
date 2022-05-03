{-# LANGUAGE RecordWildCards #-}

module BowBot.Command.HelpCommand where

import BowBot.Command
import BowBot.BotData.Info

helpCommand :: [Command] -> PermissionLevel -> Maybe (String -> String) -> String -> String -> Command
helpCommand commands level footer group name = Command CommandInfo
  { commandName = name
  , commandUsage = name
  , commandDescription = if group == "normal" then "show this help message" else "show help message for " ++ levelname ++ group ++ "commands"
  , commandPerms = level
  , commandTimeout = 2
  , commandGroup = "normal"
  } $ hNoArguments $ do
    prefix <- hInfoDB discordCommandPrefixInfo
    hRespond $ "**Bow Bot " ++ (if group == "normal" then "" else group ++ " ") ++ "help:**\n\n**" ++ levelnameupper ++ "Commands:**\n" ++
      unlines (map (helper prefix) (filter (\c -> level == commandPerms c && group == commandGroup c) (map commandInfo commands))) ++
      maybe "" (("\n"++) . ($ prefix)) footer ++
      "\nMade by **GregC**#9698"
  where
    helper prefix CommandInfo {..} = "  - **" ++ prefix ++ commandUsage ++ "** - *" ++ commandDescription ++ "*"
    levelname = case level of
      ModLevel -> "mod "
      AdminLevel -> "admin "
      _ -> ""
    levelnameupper = case level of
      ModLevel -> "Mod "
      AdminLevel -> "Admin "
      _ -> ""