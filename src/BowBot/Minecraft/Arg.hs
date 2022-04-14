{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module BowBot.Minecraft.Arg where

import BowBot.Command.Args
import BowBot.Minecraft.Account
import Control.Monad.Except
import Control.Monad.Trans.Except
import BowBot.DB.Entity

newtype MinecraftAccountArg = MinecraftAccountArg { minecraftAccountArgName :: String }

instance CommandArg MinecraftAccountArg MinecraftAccount where
  parseArgFromStrings MinecraftAccountArg {..} [] = lift $ throwE ("*Argument not provided: " ++ minecraftAccountArgName ++ "!*")
  parseArgFromStrings _ (a:as) = do
    acc' <- hFilterFromDB emptyFilter { mcfName = Just a }
    case acc' of
      [acc] -> return (acc, as)
      _ -> do
        uuid <- maybe (lift $ throwE "*The player doesn't exist!*") return =<< mcNameToUUID a
        names <- maybe (lift $ throwE "*The player doesn't exist!*") return =<< mcUUIDToNames uuid
        return (MinecraftAccount { mcUUID = uuid, mcNames = names, mcHypixelBow = NotBanned }, as)
      -- TODO: add autocorrect
