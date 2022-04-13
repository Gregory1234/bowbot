{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BowBot.Discord.Monad where

import Discord (DiscordHandler)
import BowBot.Discord.Class
import BowBot.Utils

newtype DiscordHandler' a = DiscordHandler' { runDiscordHandler' :: DiscordHandler a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadHoistIO)

instance MonadDiscord DiscordHandler' where
  liftDiscord = DiscordHandler'