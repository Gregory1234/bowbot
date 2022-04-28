module BowBot.Discord.Commands where

import BowBot.Command
import BowBot.Discord.Roles
import BowBot.Discord.Account

updateRolesCommand :: Command () ()
updateRolesCommand = Command () CommandInfo
  { commandName = "rolesrefresh"
  , commandDescription = "" -- TODO
  , commandPerms = AdminLevel
  , commandTimeout = 120
  } $ withArgs $ \() -> do
    hRespond "Received"
    updateRolesAll
    hRespond "Done"

updateDiscordsCommand :: Command () ()
updateDiscordsCommand = Command () CommandInfo
  { commandName = "discordsupdate"
  , commandDescription = "" -- TODO
  , commandPerms = AdminLevel
  , commandTimeout = 120
  } $ withArgs $ \() -> do
    hRespond "Received"
    updateDiscordAccounts
    hRespond "Done"