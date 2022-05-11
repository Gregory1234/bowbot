{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module BowBot.Command.Args where

import BowBot.Command.Handler
import Control.Monad.Except (ExceptT(..), runExceptT, throwError, MonadError)
import BowBot.Utils
import Discord (DiscordHandle)

withArguments :: (MonadIOReader m r, HasAll [CommandEnvironment, DiscordHandle] r) => (CommandArgs -> ExceptT String m a) -> (a -> m ()) -> m ()
withArguments parser body = do
  args <- envs envArgs
  v <- runExceptT (parser args)
  case v of
    Left e -> respond e
    Right a -> body a

withArguments' :: (MonadIOReader m r, HasAll [CommandEnvironment, DiscordHandle] r) => (CommandArgs -> ExceptT String m ()) -> m ()
withArguments' body = do
  args <- envs envArgs
  v <- runExceptT (body args)
  case v of
    Left e -> respond e
    Right _ -> pure ()

noArguments :: (MonadIOReader m r, HasAll [CommandEnvironment, DiscordHandle] r) => m () -> m ()
noArguments body = withArguments (\(CommandMessageArgs args) -> assertArgumentsCount 0 0 args) $ \() -> body

oneArgument :: (MonadIOReader m r, HasAll [CommandEnvironment, DiscordHandle] r) => (String -> ExceptT String m a) -> (a -> m ()) -> m ()
oneArgument parser = withArguments (\(CommandMessageArgs args) -> assertArgumentsCount 1 1 args >> parser (head args))

oneArgument' :: (MonadIOReader m r, HasAll [CommandEnvironment, DiscordHandle] r) => (String -> ExceptT String m ()) -> m ()
oneArgument' body = withArguments' (\(CommandMessageArgs args) -> assertArgumentsCount 1 1 args >> body (head args))

oneOptionalArgument :: (MonadIOReader m r, HasAll [CommandEnvironment, DiscordHandle] r) => (Maybe String -> ExceptT String m a) -> (a -> m ()) -> m ()
oneOptionalArgument parser = withArguments (\(CommandMessageArgs args) -> assertArgumentsCount 0 1 args >> parser (listToMaybe args))

twoArguments' :: (MonadIOReader m r, HasAll [CommandEnvironment, DiscordHandle] r) => (String -> String -> ExceptT String m ()) -> m ()
twoArguments' body = withArguments' (\(CommandMessageArgs args) -> assertArgumentsCount 2 2 args >> body (head args) (args !! 1))

twoArguments :: (MonadIOReader m r, HasAll [CommandEnvironment, DiscordHandle] r) => (String -> String -> ExceptT String m a) -> (a -> m ()) -> m ()
twoArguments parser = withArguments (\(CommandMessageArgs args) -> assertArgumentsCount 2 2 args >> parser (head args) (args !! 1))

argumentsCountMsg :: Int -> Int -> Int -> String
argumentsCountMsg mina maxa args
  | mina == maxa = "Got " ++ show args ++ " arguments, " ++ show mina ++ " expected"
  | mina == 0 = "Got " ++ show args ++ " arguments, at most " ++ show maxa ++ " expected"
  | otherwise = "Got " ++ show args ++ " arguments, between " ++ show mina ++ " and " ++ show maxa ++ " expected"

assertArgumentsCount :: MonadError String m => Int -> Int -> [String] -> m ()
assertArgumentsCount mina maxa args
  | mina <= length args, maxa >= length args = pure ()
  | otherwise = throwError $ argumentsCountMsg mina maxa (length args)