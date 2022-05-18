{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.Discord.Arg where

import BowBot.BotData.Cached
import Control.Monad.Error.Class
import BowBot.Discord.Account
import BowBot.Discord.Utils
import qualified Data.HashMap.Strict as HM

discordAccFull :: (MonadIOBotData m d r, HasCache DiscordAccount d, MonadError String m) => UserId -> Maybe String -> m DiscordAccount
discordAccFull did Nothing = discordArgSelf did
discordAccFull _ (Just did) = discordArgFromName did `orElseError` discordArg did

discordArg :: (MonadIOBotData m d r, HasCache DiscordAccount d, MonadError String m) => String -> m DiscordAccount
discordArg (readMaybe -> Just did) = liftMaybe "*The discord id doesn't exist!*" =<< getFromCache did
discordArg (fromPingDiscordUser -> Just did) = liftMaybe "*The discord id doesn't exist!*" =<< getFromCache did
discordArg _ = throwError "*The discord id is invalid!*"

discordArgSelf :: (MonadIOBotData m d r, HasCache DiscordAccount d, MonadError String m) => UserId -> m DiscordAccount
discordArgSelf did = liftMaybe "*The discord id doesn't exist!*" =<< getFromCache did

discordArgFromName :: (MonadIOBotData m d r, HasCache DiscordAccount d, MonadError String m) => String -> m DiscordAccount
discordArgFromName name = liftMaybe "*The discord user doesn't exist!*" . find helper . HM.elems =<< getCacheMap
  where
    helper x = map toLower name `elem` [map toLower y | discordIsMember x, Just y <- [Just (discordName x), discordNickname x]]
