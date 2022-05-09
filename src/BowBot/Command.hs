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
import Control.Monad.Reader (runReaderT)
  
data Command = Command { commandInfo :: CommandInfo, commandHandler :: CommandHandler () }

runCommand :: Command -> Message -> Bot ()
runCommand Command {..} m = do
  ctx <- ask
  liftIO $ runReaderT commandHandler (commandEnvFromMessage m, ctx)

somethingWentWrongMessage :: String
somethingWentWrongMessage = "**Something went wrong!**"