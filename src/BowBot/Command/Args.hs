{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}

module BowBot.Command.Args(module BowBot.Command.Args, Only(..)) where

import Database.MySQL.Simple (Only(..))
import BowBot.BotMonad
import Control.Monad.Except
import Discord
import Discord.Types
import Network.HTTP.Conduit (Manager)
import BowBot.Network.Class (MonadNetwork)
import BowBot.Discord.Class (MonadDiscord)
import Control.Monad.Reader (ReaderT(..), MonadReader)
import Control.Applicative (Alternative)
import BowBot.BotData.Basic
import BowBot.BotData.Cached (MonadCache)

data ArgsParserContext = ArgsParserContext { argsParserSender :: User, argsParserChannel :: ChannelId }

newtype ArgsParser a = ArgsParser { runArgsParser :: ArgsParserContext -> BotData -> Manager -> DiscordHandle -> IO (Either String a) }
  deriving (Functor, Applicative, Monad, MonadIO, MonadNetwork, MonadDiscord, MonadError String,
            MonadFail, MonadFix, Alternative, MonadPlus, MonadReader ArgsParserContext) via (ReaderT ArgsParserContext (BotT (ExceptT String IO)))

deriving via (ReaderT ArgsParserContext (BotT (ExceptT String IO))) instance (MonadCache c (BotT (ExceptT String IO))) => MonadCache c ArgsParser

class CommandArgs a v | a -> v where
  parseArgsFromStrings :: a -> [String] -> ArgsParser v

class CommandArg a v | a -> v where
  parseArgFromStrings :: a -> [String] -> (ArgsParser v, [String])

newtype SingleStringArg = SingleStringArg { singleStringArgName :: String }

instance CommandArg SingleStringArg String where
  parseArgFromStrings SingleStringArg {..} [] = (throwError $ "*Argument not provided: " ++ singleStringArgName ++ "!*", [])
  parseArgFromStrings _ (a:as) = (return a, as)

newtype GreedyStringArg = GreedyStringArg { greedyStringArgName :: String }

instance CommandArg GreedyStringArg String where
  parseArgFromStrings GreedyStringArg {..} [] = (throwError $ "*Argument not provided: " ++ greedyStringArgName ++ "!*", [])
  parseArgFromStrings _ as = (return (unwords as), [])

instance CommandArgs () () where
  parseArgsFromStrings _ [] = return ()
  parseArgsFromStrings _ _ = throwError "*Too many arguments!*"

instance (CommandArg a1 v1) => CommandArgs (Only a1) (Only v1) where
  parseArgsFromStrings (Only a1) as = do
    let (b1, r1) = parseArgFromStrings a1 as
    () <- parseArgsFromStrings () r1
    Only <$> b1

instance (CommandArg a1 v1, CommandArg a2 v2) => CommandArgs (a1, a2) (v1, v2) where
  parseArgsFromStrings (a1, a2) as = do
    let (b1, r1) = parseArgFromStrings a1 as
    let (b2, r2) = parseArgFromStrings a2 r1
    () <- parseArgsFromStrings () r2
    (,) <$> b1 <*> b2

instance (CommandArg a1 v1, CommandArg a2 v2, CommandArg a3 v3) => CommandArgs (a1, a2, a3) (v1, v2, v3) where
  parseArgsFromStrings (a1, a2, a3) as = do
    let (b1, r1) = parseArgFromStrings a1 as
    let (b2, r2) = parseArgFromStrings a2 r1
    let (b3, r3) = parseArgFromStrings a3 r2
    () <- parseArgsFromStrings () r3
    (,,) <$> b1 <*> b2 <*> b3

instance (CommandArg a1 v1, CommandArg a2 v2, CommandArg a3 v3, CommandArg a4 v4) => CommandArgs (a1, a2, a3, a4) (v1, v2, v3, v4) where
  parseArgsFromStrings (a1, a2, a3, a4) as = do
    let (b1, r1) = parseArgFromStrings a1 as
    let (b2, r2) = parseArgFromStrings a2 r1
    let (b3, r3) = parseArgFromStrings a3 r2
    let (b4, r4) = parseArgFromStrings a4 r3
    () <- parseArgsFromStrings () r4
    (,,,) <$> b1 <*> b2 <*> b3 <*> b4