{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.Text as T

data MinecraftResponseTime = CurrentResponse | OldResponse Text deriving (Show, Eq)

data MinecraftResponseAutocorrect = ResponseAutocorrect | ResponseTrue | ResponseNew deriving (Show, Eq)

data MinecraftResponse = MinecraftResponse
  { mcResponseTime :: !MinecraftResponseTime
  , mcResponseAutocorrect :: !MinecraftResponseAutocorrect
  , mcResponseAccount :: !MinecraftAccount
  } deriving (Show, Eq)

thePlayerDoesNotExistMessage :: Text
thePlayerDoesNotExistMessage = "*The player doesn't exist!*"

minecraftArgFromCache :: (MonadError Text m, MonadIOBotData m d r, HasCache MinecraftAccount d) => Text -> m MinecraftAccount
minecraftArgFromCache (uuidFromString -> Just uuid) = liftMaybe thePlayerDoesNotExistMessage =<< getFromCache uuid
minecraftArgFromCache name = liftMaybe thePlayerDoesNotExistMessage =<< getMinecraftAccountByCurrentNameFromCache name

minecraftArgFromCacheAutocorrect :: (MonadError Text m, MonadIOBotData m d r, HasCache MinecraftAccount d) => Text -> m MinecraftResponse
minecraftArgFromCacheAutocorrect (uuidFromString -> Just uuid) = fmap (MinecraftResponse CurrentResponse ResponseTrue) $ liftMaybe thePlayerDoesNotExistMessage =<< getFromCache uuid
minecraftArgFromCacheAutocorrect name = do
  people <- HM.elems <$> getCacheMap
  let process f = let
        nicks = [(mcUUID,u) | MinecraftAccount {..} <- people, u <- f mcNames]
        dists = map (\(u,n) -> ((u, n), dist (T.toLower n) (T.toLower name))) nicks
          in map fst . sortOn snd . filter (\(_,d) -> d <= 2) $ dists
  (uuid, n, mcResponseTime) <- liftMaybe thePlayerDoesNotExistMessage $ listToMaybe
             ( map (\(uuid, n) -> (uuid, n, CurrentResponse)) (process (take 1))
            ++ map (\(uuid, n) -> (uuid, n, OldResponse n)) (process (drop 1)))
  let mcResponseAutocorrect = if T.toLower n == T.toLower name then ResponseTrue else ResponseAutocorrect
  mcResponseAccount <- liftMaybe thePlayerDoesNotExistMessage =<< getFromCache uuid
  return MinecraftResponse {..}

minecraftArgFromNetwork :: (MonadError Text m, MonadIOBotData m d r, HasCache MinecraftAccount d, Has Manager r) => Text -> m (MinecraftResponseAutocorrect, MinecraftAccount)
minecraftArgFromNetwork name = orElseError ((ResponseTrue,) <$> minecraftArgFromCache name) $ do
  mcUUID <- case name of
    (uuidFromString -> Just uuid) -> return uuid
    _ -> liftMaybe thePlayerDoesNotExistMessage =<< mcNameToUUID name
  mcNames <- liftMaybe thePlayerDoesNotExistMessage =<< mcUUIDToNames mcUUID
  return (ResponseNew, MinecraftAccount { mcHypixelBow = NotBanned, mcHypixelWatchlist = False, ..})

minecraftArgFromNetworkAutocorrect :: (MonadError Text m, MonadIOBotData m d r, HasCache MinecraftAccount d, Has Manager r) => Text -> m MinecraftResponse
minecraftArgFromNetworkAutocorrect name = flip orElseError (minecraftArgFromCacheAutocorrect name) $ do
  (mcResponseAutocorrect, mcResponseAccount) <- minecraftArgFromNetwork name
  return MinecraftResponse { mcResponseTime = CurrentResponse, ..}

data MinecraftConstraintResponse = ResponseGood | ResponseFindBetter deriving (Show, Eq)

minecraftArgConstraintAny :: (MonadError Text m) => (Text -> m MinecraftResponse) -> (Text -> m MinecraftResponse) -> (MinecraftAccount -> m (MinecraftConstraintResponse, a)) -> Text -> m (MinecraftResponse, a)
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

minecraftArgFromCacheConstraint :: (MonadError Text m, MonadIOBotData m d r, HasCache MinecraftAccount d) => (MinecraftAccount -> m (MinecraftConstraintResponse, a)) -> Text -> m (MinecraftResponse, a)
minecraftArgFromCacheConstraint = flip minecraftArgConstraintAny minecraftArgFromCacheAutocorrect $ \n -> do
  mcResponseAccount <- minecraftArgFromCache n
  return MinecraftResponse { mcResponseTime = CurrentResponse, mcResponseAutocorrect = ResponseTrue, ..}

minecraftArgFromNetworkConstraint :: (MonadError Text m, MonadIOBotData m d r, HasCache MinecraftAccount d, Has Manager r) => (MinecraftAccount -> m (MinecraftConstraintResponse, a)) -> Text -> m (MinecraftResponse, a)
minecraftArgFromNetworkConstraint = flip minecraftArgConstraintAny minecraftArgFromCacheAutocorrect $ \n -> do -- note: this is correct
  (mcResponseAutocorrect, mcResponseAccount) <- minecraftArgFromNetwork n
  return MinecraftResponse { mcResponseTime = CurrentResponse, ..}

theUserIsntRegisteredMessage :: Text
theUserIsntRegisteredMessage = "*The user isn't registered!*"

minecraftArgFromDiscord :: (MonadError Text m, MonadIOBotData m d r, HasCaches '[DiscordAccount, MinecraftAccount, BowBotAccount] d) => Text -> m MinecraftAccount
minecraftArgFromDiscord did = do
  dacc <- discordArg did
  bbacc <- liftMaybe theUserIsntRegisteredMessage =<< getBowBotAccountByDiscord (discordId dacc)
  liftMaybe theUserIsntRegisteredMessage =<< getFromCache (accountSelectedMinecraft bbacc)

youArentRegisteredMessage :: Text
youArentRegisteredMessage = "*You aren't registered! To register, type `?register yourign`.*"

minecraftArgFromDiscordSelf :: (MonadError Text m, MonadIOBotData m d r, HasCaches '[DiscordAccount, MinecraftAccount, BowBotAccount] d) => UserId -> m MinecraftAccount
minecraftArgFromDiscordSelf did = do
  dacc <- discordArgSelf did
  bbacc <- liftMaybe youArentRegisteredMessage =<< getBowBotAccountByDiscord (discordId dacc)
  liftMaybe youArentRegisteredMessage =<< getFromCache (accountSelectedMinecraft bbacc)

minecraftArgFromDiscordName :: (MonadError Text m, MonadIOBotData m d r, HasCaches '[DiscordAccount, MinecraftAccount, BowBotAccount] d) => Text -> m MinecraftAccount
minecraftArgFromDiscordName name = do
  dacc <- discordArgFromName name
  bbacc <- liftMaybe theUserIsntRegisteredMessage =<< getBowBotAccountByDiscord (discordId dacc)
  liftMaybe theUserIsntRegisteredMessage =<< getFromCache (accountSelectedMinecraft bbacc)

minecraftArgFromNetworkAutocorrectWithDiscordName :: (MonadError Text m, MonadIOBotData m d r, HasCaches '[DiscordAccount, MinecraftAccount, BowBotAccount] d, Has Manager r) => Text -> m MinecraftResponse
minecraftArgFromNetworkAutocorrectWithDiscordName name = flip orElseError (minecraftArgFromCacheAutocorrect name) $ do
  (mcResponseAutocorrect, mcResponseAccount) <- minecraftArgFromNetwork name `orElseError` ((ResponseTrue,) <$> minecraftArgFromDiscordName name)
  return MinecraftResponse { mcResponseTime = CurrentResponse, ..}

minecraftArgFromNetworkConstraintWithDiscordName :: (MonadError Text m, MonadIOBotData m d r, HasCaches '[DiscordAccount, MinecraftAccount, BowBotAccount] d, Has Manager r) => (MinecraftAccount -> m (MinecraftConstraintResponse, a)) -> Text -> m (MinecraftResponse, a)
minecraftArgFromNetworkConstraintWithDiscordName = minecraftArgConstraintAny (\n -> do -- note: this is correct
  (mcResponseAutocorrect, mcResponseAccount) <- minecraftArgFromNetwork n
  return MinecraftResponse { mcResponseTime = CurrentResponse, ..}) (\n -> flip orElseError (minecraftArgFromCacheAutocorrect n) $ do
    mcResponseAccount <- minecraftArgFromDiscordName n
    return MinecraftResponse { mcResponseTime = CurrentResponse, mcResponseAutocorrect = ResponseTrue, ..})

minecraftArgFull :: (MonadError Text m, MonadIOBotData m d r, HasCaches '[DiscordAccount, MinecraftAccount, BowBotAccount] d, Has Manager r) => UserId -> Maybe Text -> m MinecraftResponse
minecraftArgFull did Nothing = do
  mcResponseAccount <- minecraftArgFromDiscordSelf did
  return MinecraftResponse { mcResponseTime = CurrentResponse, mcResponseAutocorrect = ResponseTrue, ..}
minecraftArgFull _ (Just name) = flip orElseError (minecraftArgFromNetworkAutocorrectWithDiscordName name) $ do
  mcResponseAccount <- minecraftArgFromDiscord name
  return MinecraftResponse { mcResponseTime = CurrentResponse, mcResponseAutocorrect = ResponseTrue, ..}

minecraftArgFullConstraint :: (MonadError Text m, MonadIOBotData m d r, HasCaches '[DiscordAccount, MinecraftAccount, BowBotAccount] d, Has Manager r) => (MinecraftAccount -> m (MinecraftConstraintResponse, a)) -> UserId -> Maybe Text -> m (MinecraftResponse, a)
minecraftArgFullConstraint constraint did Nothing = do
  mcResponseAccount <- minecraftArgFromDiscordSelf did
  (_, v) <- constraint mcResponseAccount
  return (MinecraftResponse { mcResponseTime = CurrentResponse, mcResponseAutocorrect = ResponseTrue, ..}, v)
minecraftArgFullConstraint constraint _ (Just name) = flip orElseError (minecraftArgFromNetworkConstraintWithDiscordName constraint name) $ do
  mcResponseAccount <- minecraftArgFromDiscord name
  (_, v) <- constraint mcResponseAccount
  return (MinecraftResponse { mcResponseTime = CurrentResponse, mcResponseAutocorrect = ResponseTrue, ..}, v)


showMinecraftAccount :: MinecraftResponseTime -> MinecraftAccount -> Text
showMinecraftAccount CurrentResponse MinecraftAccount {..} = head mcNames
showMinecraftAccount (OldResponse o) MinecraftAccount {..} = o <> " (" <> head mcNames <> ")"

showMinecraftAccountDiscord :: MinecraftResponseTime -> MinecraftAccount -> Text
showMinecraftAccountDiscord CurrentResponse MinecraftAccount {..} = "**" <> discordEscape (head mcNames) <> "**"
showMinecraftAccountDiscord (OldResponse o) MinecraftAccount {..} = "**" <> discordEscape o <> "** (" <> discordEscape (head mcNames) <> ")"
