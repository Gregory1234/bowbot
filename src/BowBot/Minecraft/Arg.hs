{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module BowBot.Minecraft.Arg where

import BowBot.Command.Args
import BowBot.Minecraft.Account
import Control.Monad.Except
import BowBot.DB.Entity
import BowBot.Utils

newtype MinecraftAccountArg = MinecraftAccountArg { minecraftAccountArgName :: String }

instance CommandArg MinecraftAccountArg MinecraftAccount where
  parseArgFromStrings MinecraftAccountArg {..} [] = throwError $ "*Argument not provided: " ++ minecraftAccountArgName ++ "!*"
  parseArgFromStrings _ (a:as) = do
    acc' <- hFilterFromDB emptyFilter { mcfName = Just a }
    case acc' of
      [acc] -> return (acc, as)
      _ -> do
        uuid <- liftMaybe "*The player doesn't exist!*" =<< mcNameToUUID a
        names <- liftMaybe "*The player doesn't exist!*" =<< mcUUIDToNames uuid
        return (MinecraftAccount { mcUUID = uuid, mcNames = names, mcHypixelBow = NotBanned }, as)
      -- TODO: add autocorrect
