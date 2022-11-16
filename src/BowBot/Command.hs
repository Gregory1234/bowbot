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
  BotContext {..} <- ask
  liftIO $ runReaderT commandHandler CommandHandlerContext { cctxEnv = commandEnvFromMessage m, cctxManager = bctxManager, cctxConnection = bctxConnection, cctxDiscord = bctxDiscord, cctxCounter = bctxCounter, cctxData = bctxData}

somethingWentWrongMessage :: String
somethingWentWrongMessage = "**Something went wrong!**"