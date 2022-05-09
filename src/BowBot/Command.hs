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
import BowBot.Network.Basic
  
data Command = Command { commandInfo :: CommandInfo, commandHandler :: CommandHandler () }

runCommand :: Command -> Message -> Bot ()
runCommand Command {..} m = do
  bdt <- BotT $ \d _ -> return d
  manager <- asks getter
  handle <- asks getter
  liftIO $ runCommandHandler commandHandler bdt (commandEnvFromMessage m, manager, handle)

somethingWentWrongMessage :: String
somethingWentWrongMessage = "**Something went wrong!**"