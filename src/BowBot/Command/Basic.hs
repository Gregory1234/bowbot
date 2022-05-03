module BowBot.Command.Basic(module BowBot.Command.Basic, module BowBot.Perms.Basic) where

import BowBot.Perms.Basic

data HelpEntry = HelpEntry { helpUsage :: String, helpDescription :: String, helpGroup :: String } deriving (Show)

data CommandInfo = CommandInfo
  { commandName :: String
  , commandHelpEntries :: [HelpEntry]
  , commandPerms :: PermissionLevel
  , commandTimeout :: Int
  } deriving (Show)