{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module BowBot.Minecraft.Arg where

import BowBot.Command.Args
import BowBot.Minecraft.Account
import BowBot.Account.Basic
import Control.Monad.Except
import BowBot.Utils
import qualified Data.HashMap.Strict as HM
import Data.Proxy
import BowBot.BotData.Cached
import Data.Char (toLower, isDigit)
import Data.List (sortOn, isPrefixOf, isSuffixOf)
import Data.Maybe (listToMaybe)
import Control.Monad.Reader (asks)
import Discord.Types


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

thePlayerDoesNotExistMessage :: String
thePlayerDoesNotExistMessage = "*The player doesn't exist!*"

fromPingDiscordUser :: String -> Maybe UserId
fromPingDiscordUser str | "<@" `isPrefixOf` str && ">" `isSuffixOf` str = readMaybe $ filter isDigit str
fromPingDiscordUser _ = Nothing

instance CommandArg (MinecraftArg a) (MinecraftResponse a) where
  parseArgFromStrings MinecraftArg {..} [] = (, []) $ do
    user <- asks argsParserSender
    bbacc <- liftMaybe "*You aren't registered! To register, type `?register yourign`.*" =<< getBowBotAccountByDiscord (userId user)
    acc <- liftMaybe thePlayerDoesNotExistMessage =<< getFromCache (Proxy @MinecraftAccount) (accountSelectedMinecraft bbacc)
    val <- minecraftArgFilter acc
    return MinecraftResponse { responseType = JustResponse, responseAccount = acc, responseValue = val }
  parseArgFromStrings MinecraftArg {..} ((fromPingDiscordUser -> Just did):as) = (, as) $ do
    bbacc <- liftMaybe "*The user isn't registered!*" =<< getBowBotAccountByDiscord did
    acc <- liftMaybe thePlayerDoesNotExistMessage =<< getFromCache (Proxy @MinecraftAccount) (accountSelectedMinecraft bbacc)
    val <- minecraftArgFilter acc
    return MinecraftResponse { responseType = JustResponse, responseAccount = acc, responseValue = val }
  parseArgFromStrings MinecraftArg {..} (a:as) = (,as) $ do
    acc' <- filter ((==map toLower a) . map toLower . head . mcNames) . HM.elems <$> getCacheMap (Proxy @MinecraftAccount)
    case acc' of
      [acc] -> do
        val <- minecraftArgFilter acc
        return MinecraftResponse { responseType = JustResponse, responseAccount = acc, responseValue = val }
      _ -> catchError (do
            uuid <- liftMaybe thePlayerDoesNotExistMessage =<< mcNameToUUID a
            names <- liftMaybe thePlayerDoesNotExistMessage =<< mcUUIDToNames uuid
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
                names <- liftMaybe thePlayerDoesNotExistMessage =<< mcUUIDToNames uuid
                let acc = MinecraftAccount { mcUUID = uuid, mcNames = names, mcHypixelBow = NotBanned }
                val <- minecraftArgFilter acc
                let rtype = if map toLower n == map toLower a then JustResponse else DidYouMeanResponse
                return MinecraftResponse { responseType = rtype, responseAccount = acc, responseValue = val }
              Nothing -> case listToMaybe $ process (drop 1) of
                Just (uuid, n) -> do
                  names <- liftMaybe thePlayerDoesNotExistMessage =<< mcUUIDToNames uuid
                  let acc = MinecraftAccount { mcUUID = uuid, mcNames = names, mcHypixelBow = NotBanned }
                  val <- minecraftArgFilter acc
                  let rtype = if map toLower n == map toLower a then OldResponse n else DidYouMeanOldResponse n
                  return MinecraftResponse { responseType = rtype, responseAccount = acc, responseValue = val }
                Nothing -> throwError e