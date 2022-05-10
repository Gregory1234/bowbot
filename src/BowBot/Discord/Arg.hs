{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.Discord.Arg where

import BowBot.BotData.Cached
import Control.Monad.Error.Class
import BowBot.Discord.Account
import BowBot.Discord.Utils



discordArg :: (MonadIO m, MonadReader r m, HasBotData d r, HasCache DiscordAccount d, MonadError String m) => String -> m DiscordAccount
discordArg (readMaybe -> Just did) = liftMaybe "*The discord id doesn't exist!*" =<< getFromCache did
discordArg (fromPingDiscordUser -> Just did) = liftMaybe "*The discord id doesn't exist!*" =<< getFromCache did
discordArg _ = throwError "*The discord id is invalid!*"