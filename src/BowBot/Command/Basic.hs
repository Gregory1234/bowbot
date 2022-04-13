module BowBot.Command.Basic(module BowBot.Command.Basic, module BowBot.Perms.Basic) where

import BowBot.Perms.Basic
  
data CommandInfo = CommandInfo
  { commandName :: String
  , commandDescription :: String
  , commandPerms :: PermissionLevel
  , commandTimeout :: Int
  } deriving (Show)