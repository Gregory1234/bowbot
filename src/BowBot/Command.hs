{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module BowBot.Command(
  module BowBot.Command, module BowBot.Command.Basic, module BowBot.Command.Handler, module BowBot.Command.Args
) where

import BowBot.Command.Basic
import BowBot.Command.Handler
import BowBot.Command.Args
import BowBot.BotMonad
import Discord.Types
import BowBot.DB.Class (hConnection)
import BowBot.Network.Class (hManager)
import BowBot.Discord.Class (liftDiscord)
  
data Command desc args = Command { commandArgsDescription :: desc, commandInfo :: CommandInfo, commandHandler :: CommandHandler args () }

data AnyCommand = forall desc args. (CommandArgs desc args) => AnyCommand { anyCommand :: Command desc args }

anyCommandInfo :: AnyCommand -> CommandInfo
anyCommandInfo AnyCommand {..} = commandInfo anyCommand

runAnyCommand :: AnyCommand -> Message -> Bot ()
runAnyCommand AnyCommand {..} m = do
  conn <- hConnection
  manager <- hManager
  liftDiscord $ runCommandHandler (commandHandler anyCommand) (commandEnvFromMessage (commandArgsDescription anyCommand) m) conn manager