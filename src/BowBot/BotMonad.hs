{-# LANGUAGE TypeOperators #-}

module BowBot.BotMonad where

import Discord
import Network.HTTP.Conduit (Manager)
import Control.Monad.Reader (ReaderT(..))
import BowBot.BotData.Basic
import Data.Has

type BotContext = Manager :*: DiscordHandle :*: BotData

type BotT = ReaderT BotContext

type Bot = BotT IO