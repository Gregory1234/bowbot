module BowBot.Command.Args where

import BowBot.Command.Handler
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Data.Maybe (listToMaybe)

hWithArguments :: (CommandArgs -> ExceptT String CommandHandler a) -> (a -> CommandHandler ()) -> CommandHandler ()
hWithArguments parser body = do
  args <- hEnv envArgs
  v <- runExceptT (parser args)
  case v of
    Left e -> hRespond e
    Right a -> body a

hNoArguments :: CommandHandler () -> CommandHandler ()
hNoArguments body = hWithArguments noArgumentParser $ \() -> body

hOneArgument :: (String -> ExceptT String CommandHandler a) -> (a -> CommandHandler ()) -> CommandHandler ()
hOneArgument parser = hWithArguments (\(CommandMessageArgs args) -> assertArgumentsCount 1 1 args >> parser (head args))

hOneOptionalArgument :: (Maybe String -> ExceptT String CommandHandler a) -> (a -> CommandHandler ()) -> CommandHandler ()
hOneOptionalArgument parser = hWithArguments (\(CommandMessageArgs args) -> assertArgumentsCount 0 1 args >> parser (listToMaybe args))

noArgumentParser :: CommandArgs -> ExceptT String CommandHandler ()
noArgumentParser (CommandMessageArgs args) = assertArgumentsCount 0 0 args

assertArgumentsCount :: Int -> Int -> [String] -> ExceptT String CommandHandler ()
assertArgumentsCount mina maxa args
  | mina <= length args, maxa >= length args = pure ()
  | mina == maxa = throwError $ "Got " ++ show (length args) ++ " arguments, " ++ show mina ++ " expected"
  | mina == 0 = throwError $ "Got " ++ show (length args) ++ " arguments, at most " ++ show maxa ++ " expected"
  | otherwise = throwError $ "Got " ++ show (length args) ++ " arguments, between " ++ show mina ++ " and " ++ show maxa ++ " expected"