{-# LANGUAGE FunctionalDependencies #-}

module BowBot.BotData.HasData where

import Data.Has
import Control.Monad.Reader (MonadReader, MonadIO)
import BowBot.HoistIO (MonadHoistIO)

class Has d r => HasBotData d r | r -> d

instance HasBotData d (a, d)

getterData :: forall a d r. (HasBotData d r, Has a d) => r -> a
getterData = getter @a . getter @d
modifierData :: forall a d r. (HasBotData d r, Has a d) => (a -> a) -> r -> r
modifierData = modifier @d . modifier @a

type MonadIOBotData m d r = (MonadIO m, MonadReader r m, HasBotData d r)
type MonadHoistIOBotData m d r = (MonadHoistIO m, MonadReader r m, HasBotData d r)