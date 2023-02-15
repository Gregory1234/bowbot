{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.Command.Args where

import BowBot.Command.Handler
import Control.Monad.Except (ExceptT(..), runExceptT, throwError, MonadError)
import BowBot.Utils
import Discord (DiscordHandle)

withArguments :: (MonadIOReader m r, HasAll [CommandEnvironment, DiscordHandle] r) => (CommandArgs -> ExceptT Text m a) -> (a -> m ()) -> m ()
withArguments parser body = do
  args <- envs envArgs
  v <- runExceptT (parser args)
  case v of
    Left e -> respond e
    Right a -> body a

withArguments' :: (MonadIOReader m r, HasAll [CommandEnvironment, DiscordHandle] r) => (CommandArgs -> ExceptT Text m ()) -> m ()
withArguments' body = do
  args <- envs envArgs
  v <- runExceptT (body args)
  case v of
    Left e -> respond e
    Right _ -> pure ()

noArguments :: (MonadIOReader m r, HasAll [CommandEnvironment, DiscordHandle] r) => m () -> m ()
noArguments body = withArguments (\(CommandMessageArgs args) -> assertArgumentsCount 0 0 args) $ \() -> body

oneArgument :: (MonadIOReader m r, HasAll [CommandEnvironment, DiscordHandle] r) => (Text -> ExceptT Text m a) -> (a -> m ()) -> m ()
oneArgument parser = withArguments (\(CommandMessageArgs args) -> assertArgumentsCount 1 1 args >> parser (head args))

oneArgument' :: (MonadIOReader m r, HasAll [CommandEnvironment, DiscordHandle] r) => (Text -> ExceptT Text m ()) -> m ()
oneArgument' body = withArguments' (\(CommandMessageArgs args) -> assertArgumentsCount 1 1 args >> body (head args))

oneOptionalArgument :: (MonadIOReader m r, HasAll [CommandEnvironment, DiscordHandle] r) => (Maybe Text -> ExceptT Text m a) -> (a -> m ()) -> m ()
oneOptionalArgument parser = withArguments (\(CommandMessageArgs args) -> assertArgumentsCount 0 1 args >> parser (listToMaybe args))

twoArguments' :: (MonadIOReader m r, HasAll [CommandEnvironment, DiscordHandle] r) => (Text -> Text -> ExceptT Text m ()) -> m ()
twoArguments' body = withArguments' (\(CommandMessageArgs args) -> assertArgumentsCount 2 2 args >> body (head args) (args !! 1))

twoArguments :: (MonadIOReader m r, HasAll [CommandEnvironment, DiscordHandle] r) => (Text -> Text -> ExceptT Text m a) -> (a -> m ()) -> m ()
twoArguments parser = withArguments (\(CommandMessageArgs args) -> assertArgumentsCount 2 2 args >> parser (head args) (args !! 1))

argumentsCountMsg :: Int -> Int -> Int -> Text
argumentsCountMsg mina maxa args
  | mina == maxa = "Got " <> showt args <> " arguments, " <> showt mina <> " expected"
  | mina == 0 = "Got " <> showt args <> " arguments, at most " <> showt maxa <> " expected"
  | otherwise = "Got " <> showt args <> " arguments, between " <> showt mina <> " and " <> showt maxa <> " expected"

assertArgumentsCount :: MonadError Text m => Int -> Int -> [Text] -> m ()
assertArgumentsCount mina maxa args
  | mina <= length args, maxa >= length args = pure ()
  | otherwise = throwError $ argumentsCountMsg mina maxa (length args)