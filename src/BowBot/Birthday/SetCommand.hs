module BowBot.Birthday.SetCommand where

import BowBot.Command
import BowBot.Discord.Account
import BowBot.Discord.Utils
import BowBot.Birthday.Basic
  
setBirthdayCommand :: Command
setBirthdayCommand = Command CommandInfo
  { commandName = "bdset"
  , commandHelpEntries = [HelpEntry { helpUsage = "bdset [discord/discord id] [day(1-31).month(1-12)]", helpDescription = "override someone's birthday", helpGroup = "normal" }]
  , commandPerms = ModLevel
  , commandTimeout = 15
  } $ twoArguments $ \didStr dbStr -> do
    undefined