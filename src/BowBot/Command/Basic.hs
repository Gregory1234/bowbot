module BowBot.Command.Basic(module BowBot.Command.Basic, module BowBot.Perms.Basic) where

import BowBot.Perms.Basic
  
data CommandInfo = CommandInfo
  { commandName :: String
  , commandUsage :: String
  , commandDescription :: String
  , commandPerms :: PermissionLevel
  , commandTimeout :: Int
  , commandGroup :: String
  } deriving (Show)