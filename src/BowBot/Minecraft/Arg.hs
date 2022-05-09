{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module BowBot.Minecraft.Arg where

import BowBot.Minecraft.Account
import BowBot.Account.Basic
import Control.Monad.Except
import BowBot.Utils
import qualified Data.HashMap.Strict as HM
import BowBot.BotData.Cached
import Data.Char (toLower)
import Data.List (sortOn)
import Data.Maybe (listToMaybe, isNothing)
import BowBot.Discord.Utils
import BowBot.Network.Basic

data MinecraftResponse a = MinecraftResponse { responseType :: MinecraftResponseType, responseAccount :: MinecraftAccount, responseValue :: a }

data MinecraftResponseType
  = JustResponse
  | OldResponse String
  | DidYouMeanResponse
  | DidYouMeanOldResponse String

thePlayerDoesNotExistMessage :: String
thePlayerDoesNotExistMessage = "*The player doesn't exist!*"

minecraftArgDefault :: (MonadError String m, MonadCache MinecraftAccount m, MonadReader r m, Has Manager r, MonadCache BowBotAccount m) => (MinecraftAccount -> m (Bool, a)) -> Maybe String -> UserId -> m (MinecraftResponse a)
minecraftArgDefault arg Nothing did = minecraftArgDiscordSelf arg did
minecraftArgDefault arg (Just (fromPingDiscordUser -> Just did)) _ = minecraftArgDiscord' arg did
minecraftArgDefault arg (Just name) _ = minecraftArgNoAutocorrect' arg name

minecraftArgNoAutocorrect' :: (MonadError String m, MonadCache MinecraftAccount m, MonadReader r m, Has Manager r) => (MinecraftAccount -> m (Bool, a)) -> String -> m (MinecraftResponse a)
minecraftArgNoAutocorrect' = minecraftArgNoAutocorrect Nothing

minecraftArgNoAutocorrect :: (MonadError String m, MonadCache MinecraftAccount m, MonadReader r m, Has Manager r) => Maybe String -> (MinecraftAccount -> m (Bool, a)) -> String -> m (MinecraftResponse a)
minecraftArgNoAutocorrect err arg name = do
  acc' <- filter ((==map toLower name) . map toLower . head . mcNames) . HM.elems <$> getCacheMap
  case acc' of
    [acc] -> do
      (_, val) <- arg acc
      return MinecraftResponse { responseType = JustResponse, responseAccount = acc, responseValue = val }
    _ -> catchError (do
          uuid <- liftMaybe thePlayerDoesNotExistMessage =<< mcNameToUUID name
          names <- liftMaybe thePlayerDoesNotExistMessage =<< mcUUIDToNames uuid
          let acc = MinecraftAccount { mcUUID = uuid, mcNames = names, mcHypixelBow = NotBanned, mcHypixelWatchlist = False }
          (b, val) <- arg acc
          let res = MinecraftResponse { responseType = JustResponse, responseAccount = acc, responseValue = val }
          if not b && isNothing err
            then minecraftArgAutocorrect Nothing (Just res) arg name
            else return res
        ) $ \e -> case err of
              Nothing -> minecraftArgAutocorrect (Just e) Nothing arg name
              Just e' -> throwError e'

minecraftArgAutocorrect' :: (MonadError String m, MonadCache MinecraftAccount m, MonadReader r m, Has Manager r) => (MinecraftAccount -> m (Bool, a)) -> String -> m (MinecraftResponse a)
minecraftArgAutocorrect' = minecraftArgAutocorrect Nothing Nothing

minecraftArgAutocorrect :: (MonadError String m, MonadCache MinecraftAccount m, MonadReader r m, Has Manager r) => Maybe String -> Maybe (MinecraftResponse a) -> (MinecraftAccount -> m (Bool, a)) -> String -> m (MinecraftResponse a)
minecraftArgAutocorrect err retm arg name = do
  people <- HM.elems <$> getCacheMap
  let process f = let
        nicks = [(mcUUID,u) | MinecraftAccount {..} <- people, u <- f mcNames]
        dists = map (\(u,n) -> ((u, n), dist (map toLower n) (map toLower name))) nicks
          in map fst . sortOn snd . filter (\(_,d) -> d <= 2) $ dists
  case listToMaybe $ process (take 1) of
    Just (uuid, n) -> do
      names <- liftMaybe thePlayerDoesNotExistMessage =<< mcUUIDToNames uuid
      let acc = MinecraftAccount { mcUUID = uuid, mcNames = names, mcHypixelBow = NotBanned, mcHypixelWatchlist = False }
      (_, val) <- arg acc
      let rtype = if map toLower n == map toLower name then JustResponse else DidYouMeanResponse
      return MinecraftResponse { responseType = rtype, responseAccount = acc, responseValue = val }
    Nothing -> case listToMaybe $ process (drop 1) of
      Just (uuid, n) -> do
        names <- liftMaybe thePlayerDoesNotExistMessage =<< mcUUIDToNames uuid
        let acc = MinecraftAccount { mcUUID = uuid, mcNames = names, mcHypixelBow = NotBanned, mcHypixelWatchlist = False }
        (_, val) <- arg acc
        let rtype = if map toLower n == map toLower name then OldResponse n else DidYouMeanOldResponse n
        return MinecraftResponse { responseType = rtype, responseAccount = acc, responseValue = val }
      Nothing -> case (err, retm) of
        (Nothing, Nothing) -> minecraftArgNoAutocorrect (Just thePlayerDoesNotExistMessage) arg name
        (Just e', Nothing) -> throwError e'
        (_, Just ret) -> return ret

theUserIsntRegisteredMessage :: String
theUserIsntRegisteredMessage = "*The user isn't registered!*"

minecraftArgDiscord' :: (MonadError String m, MonadCache BowBotAccount m, MonadCache MinecraftAccount m) => (MinecraftAccount -> m (Bool, a)) -> UserId -> m (MinecraftResponse a)
minecraftArgDiscord' = minecraftArgDiscord theUserIsntRegisteredMessage

youArentRegisteredMessage :: String
youArentRegisteredMessage = "*You aren't registered! To register, type `?register yourign`.*"

minecraftArgDiscordSelf :: (MonadError String m, MonadCache BowBotAccount m, MonadCache MinecraftAccount m) => (MinecraftAccount -> m (Bool, a)) -> UserId -> m (MinecraftResponse a)
minecraftArgDiscordSelf = minecraftArgDiscord youArentRegisteredMessage

minecraftArgDiscord :: (MonadError String m, MonadCache BowBotAccount m, MonadCache MinecraftAccount m) => String -> (MinecraftAccount -> m (Bool, a)) -> UserId -> m (MinecraftResponse a)
minecraftArgDiscord err arg did = do
  bbacc <- liftMaybe err =<< getBowBotAccountByDiscord did
  acc <- liftMaybe thePlayerDoesNotExistMessage =<< getFromCache (accountSelectedMinecraft bbacc)
  (_, val) <- arg acc
  return MinecraftResponse { responseType = JustResponse, responseAccount = acc, responseValue = val }

showMinecraftAccount :: MinecraftResponseType -> MinecraftAccount -> String
showMinecraftAccount JustResponse MinecraftAccount {..} = head mcNames
showMinecraftAccount (OldResponse o) MinecraftAccount {..} = o ++ " (" ++ head mcNames ++ ")"
showMinecraftAccount DidYouMeanResponse MinecraftAccount {..} = head mcNames
showMinecraftAccount (DidYouMeanOldResponse o) MinecraftAccount {..} = o ++ " (" ++ head mcNames ++ ")"

showMinecraftAccountDiscord :: MinecraftResponseType -> MinecraftAccount -> String
showMinecraftAccountDiscord JustResponse MinecraftAccount {..} = "**" ++ discordEscape (head mcNames) ++ "**"
showMinecraftAccountDiscord (OldResponse o) MinecraftAccount {..} = "**" ++ discordEscape o ++ "** (" ++ discordEscape (head mcNames) ++ ")"
showMinecraftAccountDiscord DidYouMeanResponse MinecraftAccount {..} = "**" ++ discordEscape (head mcNames) ++ "**"
showMinecraftAccountDiscord (DidYouMeanOldResponse o) MinecraftAccount {..} = "**" ++ discordEscape o ++ "** (" ++ discordEscape (head mcNames) ++ ")"

isDidYouMean :: MinecraftResponseType -> Bool
isDidYouMean DidYouMeanResponse = True
isDidYouMean (DidYouMeanOldResponse _) = True
isDidYouMean _ = False