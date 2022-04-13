{-# LANGUAGE DerivingVia #-}

module BowBot.BotMonad where

import BowBot.Utils
import Discord
import Network.HTTP.Conduit (Manager)
import Database.MySQL.Simple (Connection)
import BowBot.Network.Class (MonadNetwork)
import BowBot.DB.Class (MonadDB)
import BowBot.Discord.Class (MonadDiscord)
import BowBot.Network.Monad (NetworkT(..))
import BowBot.DB.Monad (DatabaseT(..))
import BowBot.Discord.Monad (DiscordHandler'(..))

newtype Bot a = Bot { runBot :: Connection -> Manager -> DiscordHandler a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadHoistIO, MonadNetwork, MonadDB, MonadDiscord) via (DatabaseT (NetworkT DiscordHandler'))
