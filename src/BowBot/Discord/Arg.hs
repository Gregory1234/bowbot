{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.Discord.Arg where

import BowBot.BotData.Cached
import Control.Monad.Error.Class
import BowBot.Discord.Account
import BowBot.Discord.Utils
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

discordAccFull :: (MonadIOBotData m d r, HasCache DiscordAccount d, MonadError Text m) => UserId -> Maybe Text -> m DiscordAccount
discordAccFull did Nothing = discordArgSelf did
discordAccFull _ (Just did) = discordArgFromName did `orElseError` discordArg did

discordArg :: (MonadIOBotData m d r, HasCache DiscordAccount d, MonadError Text m) => Text -> m DiscordAccount
discordArg (readMaybe . unpack -> Just did) = liftMaybe "*The discord id doesn't exist!*" =<< getFromCache did
discordArg (fromPingDiscordUser -> Just did) = liftMaybe "*The discord id doesn't exist!*" =<< getFromCache did
discordArg _ = throwError "*The discord id is invalid!*"

discordArgSelf :: (MonadIOBotData m d r, HasCache DiscordAccount d, MonadError Text m) => UserId -> m DiscordAccount
discordArgSelf did = liftMaybe "*The discord id doesn't exist!*" =<< getFromCache did

discordArgFromName :: (MonadIOBotData m d r, HasCache DiscordAccount d, MonadError Text m) => Text -> m DiscordAccount
discordArgFromName name = liftMaybe "*The discord user doesn't exist!*" . find helper . HM.elems =<< getCacheMap
  where
    helper x = T.toLower name `elem` [T.toLower y | discordIsMember x, Just y <- [Just (discordName x), discordNickname x]]
