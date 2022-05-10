{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module BowBot.BotData.HasData where

import Data.Has

class Has d r => HasBotData d r | r -> d

getterData :: forall a d r. (HasBotData d r, Has a d) => r -> a
getterData = getter @a . getter @d
modifierData :: forall a d r. (HasBotData d r, Has a d) => (a -> a) -> r -> r
modifierData = modifier @d . modifier @a