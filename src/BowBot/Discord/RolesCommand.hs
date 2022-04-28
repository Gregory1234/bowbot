module BowBot.Discord.RolesCommand where

import BowBot.Command
import BowBot.Discord.Roles

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