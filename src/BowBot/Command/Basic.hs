module BowBot.Command.Basic(module BowBot.Command.Basic, module BowBot.Perms.Basic) where

import BowBot.Perms.Basic

import Data.Text (Text)

data HelpEntry = HelpEntry { helpUsage :: Text, helpDescription :: Text, helpGroup :: Text } deriving (Show)

data CommandInfo = CommandInfo
  { commandName :: Text
  , commandHelpEntries :: [HelpEntry]
  , commandPerms :: PermissionLevel
  , commandTimeout :: Int
  } deriving (Show)