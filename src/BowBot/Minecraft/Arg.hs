{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
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
import Data.Maybe (listToMaybe, isNothing)
import Control.Monad.Reader (asks)
import Discord.Types


data MinecraftArg a = MinecraftArg
  { minecraftArgName :: String
  , minecraftArgFilter :: MinecraftAccount -> ArgsParser (Bool, a)
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
  parseArgFromStrings arg [] = (asks (userId . argsParserSender) >>= minecraftArgDiscord "*You aren't registered! To register, type `?register yourign`.*" arg, [])
  parseArgFromStrings arg ((fromPingDiscordUser -> Just did):as) = (minecraftArgDiscord "*The user isn't registered!*" arg did, as)
  parseArgFromStrings arg (a:as) = (minecraftArgNoAutocorrect Nothing arg a,as)

minecraftArgNoAutocorrect :: Maybe String -> MinecraftArg a -> String -> ArgsParser (MinecraftResponse a)
minecraftArgNoAutocorrect err arg@MinecraftArg {..} name = do
  acc' <- filter ((==map toLower name) . map toLower . head . mcNames) . HM.elems <$> getCacheMap (Proxy @MinecraftAccount)
  case acc' of
    [acc] -> do
      (_, val) <- minecraftArgFilter acc
      return MinecraftResponse { responseType = JustResponse, responseAccount = acc, responseValue = val }
    _ -> catchError (do
          uuid <- liftMaybe thePlayerDoesNotExistMessage =<< mcNameToUUID name
          names <- liftMaybe thePlayerDoesNotExistMessage =<< mcUUIDToNames uuid
          let acc = MinecraftAccount { mcUUID = uuid, mcNames = names, mcHypixelBow = NotBanned }
          (b, val) <- minecraftArgFilter acc
          let res = MinecraftResponse { responseType = JustResponse, responseAccount = acc, responseValue = val }
          if not b && isNothing err
            then minecraftArgAutocorrect Nothing (Just res) arg name
            else return res
        ) $ \e -> case err of
              Nothing -> minecraftArgAutocorrect (Just e) Nothing arg name
              Just e' -> throwError e'

minecraftArgAutocorrect :: Maybe String -> Maybe (MinecraftResponse a) -> MinecraftArg a -> String -> ArgsParser (MinecraftResponse a)
minecraftArgAutocorrect err retm arg@MinecraftArg {..} name = do
  people <- HM.elems <$> getCacheMap (Proxy @MinecraftAccount)
  let process f = let
        nicks = [(mcUUID,u) | MinecraftAccount {..} <- people, u <- f mcNames]
        dists = map (\(u,n) -> ((u, n), dist (map toLower n) (map toLower name))) nicks
          in map fst . sortOn snd . filter (\(_,d) -> d <= 2) $ dists
  case listToMaybe $ process (take 1) of
    Just (uuid, n) -> do
      names <- liftMaybe thePlayerDoesNotExistMessage =<< mcUUIDToNames uuid
      let acc = MinecraftAccount { mcUUID = uuid, mcNames = names, mcHypixelBow = NotBanned }
      (_, val) <- minecraftArgFilter acc
      let rtype = if map toLower n == map toLower name then JustResponse else DidYouMeanResponse
      return MinecraftResponse { responseType = rtype, responseAccount = acc, responseValue = val }
    Nothing -> case listToMaybe $ process (drop 1) of
      Just (uuid, n) -> do
        names <- liftMaybe thePlayerDoesNotExistMessage =<< mcUUIDToNames uuid
        let acc = MinecraftAccount { mcUUID = uuid, mcNames = names, mcHypixelBow = NotBanned }
        (_, val) <- minecraftArgFilter acc
        let rtype = if map toLower n == map toLower name then OldResponse n else DidYouMeanOldResponse n
        return MinecraftResponse { responseType = rtype, responseAccount = acc, responseValue = val }
      Nothing -> case (err, retm) of
        (Nothing, Nothing) -> minecraftArgNoAutocorrect (Just thePlayerDoesNotExistMessage) arg name
        (Just e', Nothing) -> throwError e'
        (_, Just ret) -> return ret

minecraftArgDiscord :: String -> MinecraftArg a -> UserId -> ArgsParser (MinecraftResponse a)
minecraftArgDiscord err MinecraftArg {..} did = do
  bbacc <- liftMaybe err =<< getBowBotAccountByDiscord did
  acc <- liftMaybe thePlayerDoesNotExistMessage =<< getFromCache (Proxy @MinecraftAccount) (accountSelectedMinecraft bbacc)
  (_, val) <- minecraftArgFilter acc
  return MinecraftResponse { responseType = JustResponse, responseAccount = acc, responseValue = val }