{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}

module BowBot.Command.Args(module BowBot.Command.Args, Only(..)) where

import Database.MySQL.Simple (Only(..))


class CommandArgs a v | a -> v where
  parseArgsFromStrings :: a -> [String] -> Either String v

class CommandArg a v | a -> v where
  parseArgFromStrings :: a -> [String] -> Either String (v, [String])

newtype SingleStringArg = SingleStringArg { singleStringArgName :: String }

instance CommandArg SingleStringArg String where
  parseArgFromStrings SingleStringArg {..} [] = Left ("*Argument not provided: " ++ singleStringArgName ++ "!*")
  parseArgFromStrings _ (a:as) = Right (a, as)

newtype GreedyStringArg = GreedyStringArg { greedyStringArgName :: String }

instance CommandArg GreedyStringArg String where
  parseArgFromStrings GreedyStringArg {..} [] = Left ("*Argument not provided: " ++ greedyStringArgName ++ "!*")
  parseArgFromStrings _ as = Right (unwords as, [])

instance CommandArgs () () where
  parseArgsFromStrings _ [] = Right ()
  parseArgsFromStrings _ _ = Left "*Too many arguments!*"

instance (CommandArg a1 v1) => CommandArgs (Only a1) (Only v1) where
  parseArgsFromStrings (Only a1) as = do
    (v1, r) <- parseArgFromStrings a1 as
    () <- parseArgsFromStrings () r
    return (Only v1)

instance (CommandArg a1 v1, CommandArg a2 v2) => CommandArgs (a1, a2) (v1, v2) where
  parseArgsFromStrings (a1, a2) as = do
    (v1, r) <- parseArgFromStrings a1 as
    Only v2 <- parseArgsFromStrings (Only a2) r
    return (v1, v2)

instance (CommandArg a1 v1, CommandArg a2 v2, CommandArg a3 v3) => CommandArgs (a1, a2, a3) (v1, v2, v3) where
  parseArgsFromStrings (a1, a2, a3) as = do
    (v1, r) <- parseArgFromStrings a1 as
    (v2, v3) <- parseArgsFromStrings (a2, a3) r
    return (v1, v2, v3)

instance (CommandArg a1 v1, CommandArg a2 v2, CommandArg a3 v3, CommandArg a4 v4) => CommandArgs (a1, a2, a3, a4) (v1, v2, v3, v4) where
  parseArgsFromStrings (a1, a2, a3, a4) as = do
    (v1, r) <- parseArgFromStrings a1 as
    (v2, v3, v4) <- parseArgsFromStrings (a2, a3, a4) r
    return (v1, v2, v3, v4)