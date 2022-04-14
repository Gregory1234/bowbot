{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

module BowBot.Minecraft.Arg where

import BowBot.Command.Args
import BowBot.Minecraft.Account
import Control.Monad.Except
import BowBot.Utils
import qualified Data.HashMap.Strict as HM
import Data.Proxy
import BowBot.BotData.Cached (getCacheMap)
import Data.Char (toLower)
import Data.List (sortOn)
import Data.Maybe (listToMaybe)


data MinecraftArg a = MinecraftArg
  { minecraftArgName :: String
  , minecraftArgFilter :: MinecraftAccount -> ArgsParser a
  }

data MinecraftResponse a = MinecraftResponse { responseType :: MinecraftResponseType, responseAccount :: MinecraftAccount, responseValue :: a }

data MinecraftResponseType
  = JustResponse
  | OldResponse String
  | DidYouMeanResponse
  | DidYouMeanOldResponse String

instance CommandArg (MinecraftArg a) (MinecraftResponse a) where
  parseArgFromStrings MinecraftArg {..} [] = (throwError $ "*Argument not provided: " ++ minecraftArgName ++ "!*", [])
  parseArgFromStrings MinecraftArg {..} (a:as) = (,as) $ do
    acc' <- filter ((==map toLower a) . map toLower . head . mcNames) . HM.elems <$> getCacheMap (Proxy @MinecraftAccount)
    case acc' of
      [acc] -> do
        val <- minecraftArgFilter acc
        return MinecraftResponse { responseType = JustResponse, responseAccount = acc, responseValue = val }
      _ -> catchError (do
            uuid <- liftMaybe "*The player doesn't exist!*" =<< mcNameToUUID a
            names <- liftMaybe "*The player doesn't exist!*" =<< mcUUIDToNames uuid
            let acc = MinecraftAccount { mcUUID = uuid, mcNames = names, mcHypixelBow = NotBanned }
            val <- minecraftArgFilter acc
            return MinecraftResponse { responseType = JustResponse, responseAccount = acc, responseValue = val }
          ) $ \e -> do
            people <- HM.elems <$> getCacheMap (Proxy @MinecraftAccount)
            let process f = let
                  nicks = [(mcUUID,u) | MinecraftAccount {..} <- people, u <- f mcNames]
                  dists = map (\(u,n) -> ((u, n), dist (map toLower n) (map toLower a))) nicks
                    in map fst . sortOn snd . filter (\(_,d) -> d <= 2) $ dists
            case listToMaybe $ process (take 1) of
              Just (uuid, n) -> do
                names <- liftMaybe "*The player doesn't exist!*" =<< mcUUIDToNames uuid
                let acc = MinecraftAccount { mcUUID = uuid, mcNames = names, mcHypixelBow = NotBanned }
                val <- minecraftArgFilter acc
                let rtype = if map toLower n == map toLower a then JustResponse else DidYouMeanResponse
                return MinecraftResponse { responseType = rtype, responseAccount = acc, responseValue = val }
              Nothing -> case listToMaybe $ process (drop 1) of
                Just (uuid, n) -> do
                  names <- liftMaybe "*The player doesn't exist!*" =<< mcUUIDToNames uuid
                  let acc = MinecraftAccount { mcUUID = uuid, mcNames = names, mcHypixelBow = NotBanned }
                  val <- minecraftArgFilter acc
                  let rtype = if map toLower n == map toLower a then OldResponse n else DidYouMeanOldResponse n
                  return MinecraftResponse { responseType = rtype, responseAccount = acc, responseValue = val }
                Nothing -> throwError e