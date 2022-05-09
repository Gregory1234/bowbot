module BowBot.Birthday.SetCommand where

import BowBot.Command
import BowBot.Discord.Arg
import BowBot.Discord.Account
import BowBot.Discord.Utils
import BowBot.Birthday.Basic
  
setBirthdayCommand :: Command
setBirthdayCommand = Command CommandInfo
  { commandName = "bdset"
  , commandHelpEntries = [HelpEntry { helpUsage = "?bdset [discord/discord id] [day(1-31).month(1-12)]", helpDescription = "override someone's birthday", helpGroup = "normal" }]
  , commandPerms = ModLevel
  , commandTimeout = 15
  } $ twoArguments (\did db -> (,) <$> (discordId <$> discordArg did) <*> liftMaybe "*Invalid birthday date!*" (birthdayFromString db)) $ \(did, db) -> do
    a <- setBirthday did db
    respond $ if a then "*Birthday set!*" else somethingWentWrongMessage