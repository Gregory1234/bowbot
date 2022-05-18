{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module BowBot.Minecraft.Arg where

import BowBot.Minecraft.Account
import Control.Monad.Except
import BowBot.Utils
import qualified Data.HashMap.Strict as HM
import BowBot.BotData.Cached
import BowBot.Network.Basic
import BowBot.Discord.Account
import BowBot.Discord.Arg
import BowBot.Account.Basic
import BowBot.Minecraft.Basic
import Discord.Types (UserId)

data MinecraftResponseTime = CurrentResponse | OldResponse String deriving (Show, Eq)

data MinecraftResponseAutocorrect = ResponseAutocorrect | ResponseTrue | ResponseNew deriving (Show, Eq)

data MinecraftResponse = MinecraftResponse
  { mcResponseTime :: MinecraftResponseTime
  , mcResponseAutocorrect :: MinecraftResponseAutocorrect
  , mcResponseAccount :: MinecraftAccount
  } deriving (Show, Eq)

thePlayerDoesNotExistMessage :: String
thePlayerDoesNotExistMessage = "*The player doesn't exist!*"

minecraftArgFromCache :: (MonadError String m, MonadIOBotData m d r, HasCache MinecraftAccount d) => String -> m MinecraftAccount
minecraftArgFromCache (uuidFromString -> Just uuid) = liftMaybe thePlayerDoesNotExistMessage =<< getFromCache uuid
minecraftArgFromCache name = liftMaybe thePlayerDoesNotExistMessage =<< getMinecraftAccountByCurrentNameFromCache name

minecraftArgFromCacheAutocorrect :: (MonadError String m, MonadIOBotData m d r, HasCache MinecraftAccount d) => String -> m MinecraftResponse
minecraftArgFromCacheAutocorrect (uuidFromString -> Just uuid) = fmap (MinecraftResponse CurrentResponse ResponseTrue) $ liftMaybe thePlayerDoesNotExistMessage =<< getFromCache uuid
minecraftArgFromCacheAutocorrect name = do
  people <- HM.elems <$> getCacheMap
  let process f = let
        nicks = [(mcUUID,u) | MinecraftAccount {..} <- people, u <- f mcNames]
        dists = map (\(u,n) -> ((u, n), dist (map toLower n) (map toLower name))) nicks
          in map fst . sortOn snd . filter (\(_,d) -> d <= 2) $ dists
  (uuid, n, mcResponseTime) <- liftMaybe thePlayerDoesNotExistMessage $ listToMaybe
             ( map (\(uuid, n) -> (uuid, n, CurrentResponse)) (process (take 1))
            ++ map (\(uuid, n) -> (uuid, n, OldResponse n)) (process (drop 1)))
  let mcResponseAutocorrect = if map toLower n == map toLower name then ResponseTrue else ResponseAutocorrect
  mcResponseAccount <- liftMaybe thePlayerDoesNotExistMessage =<< getFromCache uuid
  return MinecraftResponse {..}

minecraftArgFromNetwork :: (MonadError String m, MonadIOBotData m d r, HasCache MinecraftAccount d, Has Manager r) => String -> m (MinecraftResponseAutocorrect, MinecraftAccount)
minecraftArgFromNetwork name = orElseError ((ResponseTrue,) <$> minecraftArgFromCache name) $ do
  mcUUID <- case name of
    (uuidFromString -> Just uuid) -> return uuid
    _ -> liftMaybe thePlayerDoesNotExistMessage =<< mcNameToUUID name
  mcNames <- liftMaybe thePlayerDoesNotExistMessage =<< mcUUIDToNames mcUUID
  return (ResponseNew, MinecraftAccount { mcHypixelBow = NotBanned, mcHypixelWatchlist = False, ..})

minecraftArgFromNetworkAutocorrect :: (MonadError String m, MonadIOBotData m d r, HasCache MinecraftAccount d, Has Manager r) => String -> m MinecraftResponse
minecraftArgFromNetworkAutocorrect name = flip orElseError (minecraftArgFromCacheAutocorrect name) $ do
  (mcResponseAutocorrect, mcResponseAccount) <- minecraftArgFromNetwork name
  return MinecraftResponse { mcResponseTime = CurrentResponse, ..}

data MinecraftConstraintResponse = ResponseGood | ResponseFindBetter deriving (Show, Eq)

minecraftArgConstraintAny :: (MonadError String m) => (String -> m MinecraftResponse) -> (String -> m MinecraftResponse) -> (MinecraftAccount -> m (MinecraftConstraintResponse, a)) -> String -> m (MinecraftResponse, a)
minecraftArgConstraintAny argnoac argac constraint name = do
    first <- catchErrorEither noAutocorrect
    case first of
      Right (ResponseGood, r, v) -> return (r, v)
      _ -> do
        other <- catchErrorEither autocorrect
        case (first, other) of
          (Right (_, r, v), Right (ResponseFindBetter, _, _)) -> return (r, v)
          (Right (_, r, v), Left _) -> return (r, v)
          (Left e, Left _) -> throwError e
          (_, Right (_, r, v)) -> return (r, v)
  where
    noAutocorrect = do
      res <- argnoac name
      (rt, v) <- constraint (mcResponseAccount res)
      return (rt, res, v)
    autocorrect = do
      res <- argac name
      (rt, v) <- constraint (mcResponseAccount res)
      return (rt, res, v)

minecraftArgFromCacheConstraint :: (MonadError String m, MonadIOBotData m d r, HasCache MinecraftAccount d) => (MinecraftAccount -> m (MinecraftConstraintResponse, a)) -> String -> m (MinecraftResponse, a)
minecraftArgFromCacheConstraint = flip minecraftArgConstraintAny minecraftArgFromCacheAutocorrect $ \n -> do
  mcResponseAccount <- minecraftArgFromCache n
  return MinecraftResponse { mcResponseTime = CurrentResponse, mcResponseAutocorrect = ResponseTrue, ..}

minecraftArgFromNetworkConstraint :: (MonadError String m, MonadIOBotData m d r, HasCache MinecraftAccount d, Has Manager r) => (MinecraftAccount -> m (MinecraftConstraintResponse, a)) -> String -> m (MinecraftResponse, a)
minecraftArgFromNetworkConstraint = flip minecraftArgConstraintAny minecraftArgFromCacheAutocorrect $ \n -> do -- note: this is correct
  (mcResponseAutocorrect, mcResponseAccount) <- minecraftArgFromNetwork n
  return MinecraftResponse { mcResponseTime = CurrentResponse, ..}

theUserIsntRegisteredMessage :: String
theUserIsntRegisteredMessage = "*The user isn't registered!*"

minecraftArgFromDiscord :: (MonadError String m, MonadIOBotData m d r, HasCaches '[DiscordAccount, MinecraftAccount, BowBotAccount] d) => String -> m MinecraftAccount
minecraftArgFromDiscord did = do
  dacc <- discordArg did
  bbacc <- liftMaybe theUserIsntRegisteredMessage =<< getBowBotAccountByDiscord (discordId dacc)
  liftMaybe theUserIsntRegisteredMessage =<< getFromCache (accountSelectedMinecraft bbacc)

youArentRegisteredMessage :: String
youArentRegisteredMessage = "*You aren't registered! To register, type `?register yourign`.*"

minecraftArgFromDiscordSelf :: (MonadError String m, MonadIOBotData m d r, HasCaches '[DiscordAccount, MinecraftAccount, BowBotAccount] d) => UserId -> m MinecraftAccount
minecraftArgFromDiscordSelf did = do
  dacc <- discordArgSelf did
  bbacc <- liftMaybe youArentRegisteredMessage =<< getBowBotAccountByDiscord (discordId dacc)
  liftMaybe youArentRegisteredMessage =<< getFromCache (accountSelectedMinecraft bbacc)

minecraftArgFromDiscordName :: (MonadError String m, MonadIOBotData m d r, HasCaches '[DiscordAccount, MinecraftAccount, BowBotAccount] d) => String -> m MinecraftAccount
minecraftArgFromDiscordName name = do
  dacc <- discordArgFromName name
  bbacc <- liftMaybe theUserIsntRegisteredMessage =<< getBowBotAccountByDiscord (discordId dacc)
  liftMaybe theUserIsntRegisteredMessage =<< getFromCache (accountSelectedMinecraft bbacc)

minecraftArgFromNetworkAutocorrectWithDiscordName :: (MonadError String m, MonadIOBotData m d r, HasCaches '[DiscordAccount, MinecraftAccount, BowBotAccount] d, Has Manager r) => String -> m MinecraftResponse
minecraftArgFromNetworkAutocorrectWithDiscordName name = flip orElseError (minecraftArgFromCacheAutocorrect name) $ do
  (mcResponseAutocorrect, mcResponseAccount) <- minecraftArgFromNetwork name `orElseError` ((ResponseTrue,) <$> minecraftArgFromDiscordName name)
  return MinecraftResponse { mcResponseTime = CurrentResponse, ..}

minecraftArgFromNetworkConstraintWithDiscordName :: (MonadError String m, MonadIOBotData m d r, HasCaches '[DiscordAccount, MinecraftAccount, BowBotAccount] d, Has Manager r) => (MinecraftAccount -> m (MinecraftConstraintResponse, a)) -> String -> m (MinecraftResponse, a)
minecraftArgFromNetworkConstraintWithDiscordName = minecraftArgConstraintAny (\n -> do -- note: this is correct
  (mcResponseAutocorrect, mcResponseAccount) <- minecraftArgFromNetwork n
  return MinecraftResponse { mcResponseTime = CurrentResponse, ..}) (\n -> flip orElseError (minecraftArgFromCacheAutocorrect n) $ do
    mcResponseAccount <- minecraftArgFromDiscordName n
    return MinecraftResponse { mcResponseTime = CurrentResponse, mcResponseAutocorrect = ResponseTrue, ..})

minecraftArgFull :: (MonadError String m, MonadIOBotData m d r, HasCaches '[DiscordAccount, MinecraftAccount, BowBotAccount] d, Has Manager r) => UserId -> Maybe String -> m MinecraftResponse
minecraftArgFull did Nothing = do
  mcResponseAccount <- minecraftArgFromDiscordSelf did
  return MinecraftResponse { mcResponseTime = CurrentResponse, mcResponseAutocorrect = ResponseTrue, ..}
minecraftArgFull _ (Just name) = flip orElseError (minecraftArgFromNetworkAutocorrectWithDiscordName name) $ do
  mcResponseAccount <- minecraftArgFromDiscord name
  return MinecraftResponse { mcResponseTime = CurrentResponse, mcResponseAutocorrect = ResponseTrue, ..}

minecraftArgFullConstraint :: (MonadError String m, MonadIOBotData m d r, HasCaches '[DiscordAccount, MinecraftAccount, BowBotAccount] d, Has Manager r) => (MinecraftAccount -> m (MinecraftConstraintResponse, a)) -> UserId -> Maybe String -> m (MinecraftResponse, a)
minecraftArgFullConstraint constraint did Nothing = do
  mcResponseAccount <- minecraftArgFromDiscordSelf did
  (_, v) <- constraint mcResponseAccount
  return (MinecraftResponse { mcResponseTime = CurrentResponse, mcResponseAutocorrect = ResponseTrue, ..}, v)
minecraftArgFullConstraint constraint _ (Just name) = flip orElseError (minecraftArgFromNetworkConstraintWithDiscordName constraint name) $ do
  mcResponseAccount <- minecraftArgFromDiscord name
  (_, v) <- constraint mcResponseAccount
  return (MinecraftResponse { mcResponseTime = CurrentResponse, mcResponseAutocorrect = ResponseTrue, ..}, v)


showMinecraftAccount :: MinecraftResponseTime -> MinecraftAccount -> String
showMinecraftAccount CurrentResponse MinecraftAccount {..} = head mcNames
showMinecraftAccount (OldResponse o) MinecraftAccount {..} = o ++ " (" ++ head mcNames ++ ")"

showMinecraftAccountDiscord :: MinecraftResponseTime -> MinecraftAccount -> String
showMinecraftAccountDiscord CurrentResponse MinecraftAccount {..} = "**" ++ discordEscape (head mcNames) ++ "**"
showMinecraftAccountDiscord (OldResponse o) MinecraftAccount {..} = "**" ++ discordEscape o ++ "** (" ++ discordEscape (head mcNames) ++ ")"
