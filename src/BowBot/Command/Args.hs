module BowBot.Command.Args where

import BowBot.Command.Handler
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Data.Maybe (listToMaybe)

withArguments :: (CommandArgs -> ExceptT String CommandHandler a) -> (a -> CommandHandler ()) -> CommandHandler ()
withArguments parser body = do
  args <- envs envArgs
  v <- runExceptT (parser args)
  case v of
    Left e -> respond e
    Right a -> body a

withArguments' :: (CommandArgs -> ExceptT String CommandHandler ()) -> CommandHandler ()
withArguments' body = do
  args <- envs envArgs
  v <- runExceptT (body args)
  case v of
    Left e -> respond e
    Right _ -> pure ()

noArguments :: CommandHandler () -> CommandHandler ()
noArguments body = withArguments (\(CommandMessageArgs args) -> assertArgumentsCount 0 0 args) $ \() -> body

oneArgument :: (String -> ExceptT String CommandHandler a) -> (a -> CommandHandler ()) -> CommandHandler ()
oneArgument parser = withArguments (\(CommandMessageArgs args) -> assertArgumentsCount 1 1 args >> parser (head args))

oneArgument' :: (String -> ExceptT String CommandHandler ()) -> CommandHandler ()
oneArgument' body = withArguments' (\(CommandMessageArgs args) -> assertArgumentsCount 1 1 args >> body (head args))

oneOptionalArgument :: (Maybe String -> ExceptT String CommandHandler a) -> (a -> CommandHandler ()) -> CommandHandler ()
oneOptionalArgument parser = withArguments (\(CommandMessageArgs args) -> assertArgumentsCount 0 1 args >> parser (listToMaybe args))

twoArguments' :: (String -> String -> ExceptT String CommandHandler ()) -> CommandHandler ()
twoArguments' body = withArguments' (\(CommandMessageArgs args) -> assertArgumentsCount 2 2 args >> body (head args) (args !! 1))

twoArguments :: (String -> String -> ExceptT String CommandHandler a) -> (a -> CommandHandler ()) -> CommandHandler ()
twoArguments parser = withArguments (\(CommandMessageArgs args) -> assertArgumentsCount 2 2 args >> parser (head args) (args !! 1))

assertArgumentsCount :: Int -> Int -> [String] -> ExceptT String CommandHandler ()
assertArgumentsCount mina maxa args
  | mina <= length args, maxa >= length args = pure ()
  | mina == maxa = throwError $ "Got " ++ show (length args) ++ " arguments, " ++ show mina ++ " expected"
  | mina == 0 = throwError $ "Got " ++ show (length args) ++ " arguments, at most " ++ show maxa ++ " expected"
  | otherwise = throwError $ "Got " ++ show (length args) ++ " arguments, between " ++ show mina ++ " and " ++ show maxa ++ " expected"