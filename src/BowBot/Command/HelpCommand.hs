{-# LANGUAGE RecordWildCards #-}

module BowBot.Command.HelpCommand where

import BowBot.Command
import BowBot.BotData.Info

helpCommand :: [Command] -> PermissionLevel -> Maybe (String -> String) -> String -> String -> Command
helpCommand commands level footer group name = Command CommandInfo
  { commandName = name
  , commandHelpEntries = 
    HelpEntry { helpUsage = name, helpDescription = if group == "normal" then "show this help message" else "show help message for " ++ levelname ++ group ++ " commands", helpGroup = "normal" }:
    [HelpEntry { helpUsage = name, helpDescription = "show this help message", helpGroup = group } | group /= "normal"]
  , commandPerms = level
  , commandTimeout = 2
  } $ hNoArguments $ do
    prefix <- hInfoDB discordCommandPrefixInfo
    hRespond $ "**Bow Bot " ++ (if group == "normal" then "" else group ++ " ") ++ "help:**\n\n**" ++ levelnameupper ++ "Commands:**\n" ++
      unlines ((map (helper prefix) . filter ((==group) . helpGroup) . commandHelpEntries) =<< filter (\c -> level == commandPerms c) (map commandInfo commands)) ++
      maybe "" (("\n"++) . ($ prefix)) footer ++
      "\nMade by **GregC**#9698"
  where
    helper prefix HelpEntry {..} = "  - **" ++ prefix ++ helpUsage ++ "** - *" ++ helpDescription ++ "*"
    levelname = case level of
      ModLevel -> "mod "
      AdminLevel -> "admin "
      _ -> ""
    levelnameupper = case level of
      ModLevel -> "Mod "
      AdminLevel -> "Admin "
      _ -> ""