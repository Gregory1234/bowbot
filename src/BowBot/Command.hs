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
import BowBot.Network.Class (hManager)
import BowBot.Discord.Class (liftDiscord)
  
data Command = Command { commandInfo :: CommandInfo, commandHandler :: CommandHandler () }

runCommand :: Command -> Message -> Bot ()
runCommand Command {..} m = do
  bdt <- BotT $ \d _ _ -> return d
  manager <- hManager
  liftDiscord $ runCommandHandler commandHandler (commandEnvFromMessage m) bdt manager

somethingWentWrongMessage :: String
somethingWentWrongMessage = "**Something went wrong!**"