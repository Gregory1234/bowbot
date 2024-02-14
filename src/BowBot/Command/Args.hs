module BowBot.Command.Args where

import BowBot.Command.Handler
import Control.Monad.Except (ExceptT(..), runExceptT, throwError, MonadError)
import BowBot.Utils
import Discord (DiscordHandle)

withArguments :: (MonadIOReader m r, HasAll [CommandEnvironment, DiscordHandle] r) => (CommandArgs -> ExceptT Text m a) -> m ()
withArguments body = do
  args <- envs envArgs
  v <- runExceptT (body args)
  case v of
    Left e -> respond e
    Right _ -> pure ()

noArguments :: (MonadIOReader m r, HasAll [CommandEnvironment, DiscordHandle] r) => ExceptT Text m () -> m ()
noArguments body = withArguments (\(CommandMessageArgs args) -> assertArgumentsCount 0 0 args >> body)

oneArgument :: (MonadIOReader m r, HasAll [CommandEnvironment, DiscordHandle] r) => (Text -> ExceptT Text m ()) -> m ()
oneArgument body = withArguments (\(CommandMessageArgs args) -> assertArgumentsCount 1 1 args >> body (head args))

oneOptionalArgument :: (MonadIOReader m r, HasAll [CommandEnvironment, DiscordHandle] r) => (Maybe Text -> ExceptT Text m a) -> m ()
oneOptionalArgument body = withArguments (\(CommandMessageArgs args) -> assertArgumentsCount 0 1 args >> body (listToMaybe args))

twoArguments :: (MonadIOReader m r, HasAll [CommandEnvironment, DiscordHandle] r) => (Text -> Text -> ExceptT Text m ()) -> m ()
twoArguments body = withArguments (\(CommandMessageArgs args) -> assertArgumentsCount 2 2 args >> body (head args) (args !! 1))

twoOptionalArguments :: (MonadIOReader m r, HasAll [CommandEnvironment, DiscordHandle] r) => (Maybe (Text, Maybe Text) -> ExceptT Text m a) -> m ()
twoOptionalArguments body = withArguments (\(CommandMessageArgs args) -> assertArgumentsCount 0 2 args >> body (helper args))
  where
    helper [] = Nothing
    helper [a] = Just (a, Nothing)
    helper (a:b:_) = Just (a, Just b)

argumentsCountMsg :: Int -> Int -> Int -> Text
argumentsCountMsg mina maxa args
  | mina == maxa = "Got " <> showt args <> " arguments, " <> showt mina <> " expected"
  | mina == 0 = "Got " <> showt args <> " arguments, at most " <> showt maxa <> " expected"
  | otherwise = "Got " <> showt args <> " arguments, between " <> showt mina <> " and " <> showt maxa <> " expected"

assertArgumentsCount :: MonadError Text m => Int -> Int -> [Text] -> m ()
assertArgumentsCount mina maxa args
  | mina <= length args, maxa >= length args = pure ()
  | otherwise = throwError $ argumentsCountMsg mina maxa (length args)