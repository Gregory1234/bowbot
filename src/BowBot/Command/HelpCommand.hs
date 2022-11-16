{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.Command.HelpCommand where

import BowBot.Command
import BowBot.BotData.Info
import BowBot.Discord.Utils (Text)
import qualified Data.Text as T

helpCommand :: [Command] -> PermissionLevel -> Maybe (Text -> Text) -> Text -> Text -> Command
helpCommand commands level footer group name = Command CommandInfo
  { commandName = name
  , commandHelpEntries = 
    HelpEntry { helpUsage = name, helpDescription = if group == "normal" then "show this help message" else "show help message for " <> levelname <> group <> " commands", helpGroup = "normal" }:
    [HelpEntry { helpUsage = name, helpDescription = "show this help message", helpGroup = group } | group /= "normal"]
  , commandPerms = level
  , commandTimeout = 2
  } $ noArguments $ do
    prefix <- askInfo discordCommandPrefixInfo
    respond $ "**Bow Bot " <> (if group == "normal" then "" else group <> " ") <> "help:**\n\n**" <> levelnameupper <> "Commands:**\n" <>
      T.unlines ((map (helper prefix) . filter ((==group) . helpGroup) . commandHelpEntries) =<< filter (\c -> level == commandPerms c) (map commandInfo commands)) <>
      maybe "" (("\n"<>) . ($ prefix)) footer <>
      "\nMade by **GregC**#9698"
  where
    helper prefix HelpEntry {..} = "  - **" <> prefix <> helpUsage <> "** - *" <> helpDescription <> "*"
    levelname = case level of
      ModLevel -> "mod "
      AdminLevel -> "admin "
      _ -> ""
    levelnameupper = case level of
      ModLevel -> "Mod "
      AdminLevel -> "Admin "
      _ -> ""