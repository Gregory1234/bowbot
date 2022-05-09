{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.Account.Arg where

import BowBot.Minecraft.Account
import BowBot.Discord.Account
import BowBot.Account.Basic
import qualified Data.HashMap.Strict as HM
import BowBot.Discord.Utils
import BowBot.BotData.Cached
import Control.Monad.Except
import BowBot.Minecraft.Arg
import Data.Char (toLower)
import Data.List (sortOn)
import Data.Maybe (listToMaybe)

data AccountResponse = AccountResponse { accResponseCause :: Either DiscordAccount (MinecraftResponseType, MinecraftAccount), accResponseAccount :: BowBotAccount }

accountArgDefault :: (MonadError String m, MonadIO m, MonadReader r m, HasCache MinecraftAccount r, HasCache DiscordAccount r, HasCache BowBotAccount r) => Maybe String -> UserId -> m AccountResponse
accountArgDefault Nothing did = accountArgDiscordSelf did
accountArgDefault (Just (fromPingDiscordUser -> Just did)) _ = accountArgDiscord' did
accountArgDefault (Just name) _ = accountArgName name

thePlayerIsntRegisteredMessage :: String
thePlayerIsntRegisteredMessage = "*The player isn't registered!*"

accountArgName :: (MonadError String m, MonadIO m, MonadReader r m, HasCache MinecraftAccount r, HasCache BowBotAccount r) => String -> m AccountResponse
accountArgName name = do
  people <- HM.elems <$> getCacheMap
  let process f = let
        nicks = [(acc,u) | acc@MinecraftAccount {..} <- people, u <- f mcNames]
        dists = map (\(u,n) -> ((u, n), dist (map toLower n) (map toLower name))) nicks
          in map fst . sortOn snd . filter (\(_,d) -> d <= 2) $ dists
  case listToMaybe $ process (take 1) of
    Just (acc, n) -> do
      bacc <- liftMaybe thePlayerIsntRegisteredMessage =<< getBowBotAccountByMinecraft (mcUUID acc)
      let rtype = if map toLower n == map toLower name then JustResponse else DidYouMeanResponse
      return AccountResponse { accResponseCause = Right (rtype, acc), accResponseAccount = bacc }
    Nothing -> case listToMaybe $ process (drop 1) of
      Just (acc, n) -> do
        bacc <- liftMaybe thePlayerIsntRegisteredMessage =<< getBowBotAccountByMinecraft (mcUUID acc)
        let rtype = if map toLower n == map toLower name then OldResponse n else DidYouMeanOldResponse n
        return AccountResponse { accResponseCause = Right (rtype, acc), accResponseAccount = bacc }
      Nothing -> throwError thePlayerIsntRegisteredMessage

accountArgDiscord' :: (MonadError String m, MonadIO m, MonadReader r m, HasCache DiscordAccount r, HasCache BowBotAccount r) => UserId -> m AccountResponse
accountArgDiscord' = accountArgDiscord theUserIsntRegisteredMessage

accountArgDiscordSelf :: (MonadError String m, MonadIO m, MonadReader r m, HasCache DiscordAccount r, HasCache BowBotAccount r) => UserId -> m AccountResponse
accountArgDiscordSelf = accountArgDiscord youArentRegisteredMessage

accountArgDiscord :: (MonadError String m, MonadIO m, MonadReader r m, HasCache DiscordAccount r, HasCache BowBotAccount r) => String -> UserId -> m AccountResponse
accountArgDiscord err did = do
  bacc <- liftMaybe err =<< getBowBotAccountByDiscord did
  dacc <- liftMaybe err =<< getFromCache did
  return AccountResponse { accResponseCause = Left dacc, accResponseAccount = bacc }