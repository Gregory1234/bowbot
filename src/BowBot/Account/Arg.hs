{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.Account.Arg where

import BowBot.Minecraft.Account
import BowBot.Discord.Account
import BowBot.Account.Basic
import BowBot.Discord.Utils
import BowBot.BotData.Cached
import Control.Monad.Except
import BowBot.Minecraft.Arg
import BowBot.Discord.Arg
import BowBot.Network.Basic (Manager)

data AccountResponseType = AccountMinecraftResponse MinecraftResponse | AccountDiscordResponse DiscordAccount

data AccountResponse = AccountResponse { accResponseType :: AccountResponseType, accResponseAccount :: BowBotAccount }

noAccountArg :: (MonadError Text m, MonadIOBotData m d r, HasCaches '[DiscordAccount, MinecraftAccount] d, Has Manager r) => Text -> m AccountResponseType
noAccountArg name = (AccountDiscordResponse <$> (discordArg name `orElseError` discordArgFromName name)) `orElseError` (AccountMinecraftResponse <$> minecraftArgFromNetworkAutocorrect name)

noAccountArgFull :: (MonadError Text m, MonadIOBotData m d r, HasCaches '[DiscordAccount, MinecraftAccount] d, Has Manager r) => UserId -> Maybe Text -> m AccountResponseType
noAccountArgFull did Nothing = AccountDiscordResponse <$> discordArgSelf did
noAccountArgFull _ (Just name) = noAccountArg name

thePlayerIsntRegisteredMessage :: Text
thePlayerIsntRegisteredMessage = "*The player isn't registered!*"

accountArg :: (MonadError Text m, MonadIOBotData m d r, HasCaches '[DiscordAccount, MinecraftAccount, BowBotAccount] d) => Text -> m AccountResponse
accountArg name = (do
  dacc <- discordArg name `orElseError` discordArgFromName name
  acc <- liftMaybe theUserIsntRegisteredMessage =<< getBowBotAccountByDiscord (discordId dacc)
  return AccountResponse { accResponseType = AccountDiscordResponse dacc, accResponseAccount = acc }) `orElseError` (do
    (res, acc) <- flip minecraftArgFromCacheConstraint name $ \MinecraftAccount {..} -> fmap (ResponseGood,) $ liftMaybe thePlayerIsntRegisteredMessage =<< getBowBotAccountByMinecraft mcUUID
    return AccountResponse { accResponseType = AccountMinecraftResponse res, accResponseAccount = acc })

accountArgSelf :: (MonadError Text m, MonadIOBotData m d r, HasCaches '[DiscordAccount, BowBotAccount] d) => UserId -> m AccountResponse
accountArgSelf did = do
  dacc <- discordArgSelf did
  accResponseAccount <- liftMaybe youArentRegisteredMessage =<< getBowBotAccountByDiscord (discordId dacc)
  return AccountResponse { accResponseType = AccountDiscordResponse dacc, ..}

accountArgFull :: (MonadError Text m, MonadIOBotData m d r, HasCaches '[DiscordAccount, MinecraftAccount, BowBotAccount] d) => UserId -> Maybe Text -> m AccountResponse
accountArgFull did Nothing = accountArgSelf did
accountArgFull _ (Just name) = accountArg name