module BowBot.Discord.Commands where

import BowBot.Command
import BowBot.Discord.Roles

updateRolesCommand :: Command
updateRolesCommand = Command CommandInfo
  { commandName = "rolesupdate"
  , commandDescription = "" -- TODO
  , commandPerms = AdminLevel
  , commandTimeout = 120
  } $ hNoArguments $ do
    hRespond "Received"
    updateRolesAll
    hRespond "Done"
