{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module BowBot.Minecraft.Arg where

import BowBot.Command.Args
import BowBot.Minecraft.Account
import Control.Monad.Except
import BowBot.Utils
import qualified Data.HashMap.Strict as HM
import Data.Proxy
import BowBot.BotData.Cached (getCacheMap)
import Data.Char (toLower)

newtype MinecraftAccountArg = MinecraftAccountArg { minecraftAccountArgName :: String }

instance CommandArg MinecraftAccountArg MinecraftAccount where
  parseArgFromStrings MinecraftAccountArg {..} [] = throwError $ "*Argument not provided: " ++ minecraftAccountArgName ++ "!*"
  parseArgFromStrings _ (a:as) = do
    acc' <- filter ((==map toLower a) . map toLower . head . mcNames) . HM.elems <$> getCacheMap (Proxy @MinecraftAccount)
    case acc' of
      [acc] -> return (acc, as)
      _ -> do
        uuid <- liftMaybe "*The player doesn't exist!*" =<< mcNameToUUID a
        names <- liftMaybe "*The player doesn't exist!*" =<< mcUUIDToNames uuid
        return (MinecraftAccount { mcUUID = uuid, mcNames = names, mcHypixelBow = NotBanned }, as)
      -- TODO: add autocorrect
