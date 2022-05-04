{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.Discord.Arg where

import BowBot.BotData.Cached
import Control.Monad.Error.Class
import BowBot.Discord.Account
import BowBot.Discord.Utils
import Data.Proxy



discordArg :: (MonadCache DiscordAccount m, MonadError String m) => String -> m DiscordAccount
discordArg (readMaybe -> Just did) = liftMaybe "*The discord id doesn't exist!*" =<< getFromCache (Proxy @DiscordAccount) did
discordArg (fromPingDiscordUser -> Just did) = liftMaybe "*The discord id doesn't exist!*" =<< getFromCache (Proxy @DiscordAccount) did
discordArg _ = throwError "*The discord id is invalid!*"