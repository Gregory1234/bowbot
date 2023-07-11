{-# LANGUAGE TypeFamilies #-}

module BowBot.Settings.Basic(
  module BowBot.Settings.Basic, module BowBot.Settings.Single
) where

import BowBot.Settings.Single
import BowBot.Settings.Table
import BowBot.DB.Typed
import BowBot.Discord.Utils

data Settings = Settings
  { sWins :: !SettingBin, sLosses :: !SettingBin, sWLR :: !SettingTer, sWinsUntil :: !SettingTer
  , sBestStreak :: !SettingTer, sCurrentStreak :: !SettingTer, sBestDailyStreak :: !SettingTer
  , sBowHits :: !SettingBin, sBowShots :: !SettingBin, sAccuracy :: !SettingTer
  } deriving (Show, Eq)

instance QueryParams Settings where
  renderParams Settings {..} = [render sWins, render sLosses, render sWLR, render sWinsUntil
    , render sBestStreak, render sCurrentStreak, render sBestDailyStreak
    , render sBowHits, render sBowShots, render sAccuracy]
instance QueryResults Settings where
  convertResults = Settings <$> convert <*> convert <*> convert <*> convert
    <*> convert <*> convert <*> convert <*> convert <*> convert <*> convert

instance InTable SettingsTable Settings where
  columnRep = ColRep [SomeCol SettingsTWins, SomeCol SettingsTLosses, SomeCol SettingsTWLR, SomeCol SettingsTWinsUntil
    , SomeCol SettingsTBestStreak, SomeCol SettingsTCurrentStreak, SomeCol SettingsTBestDailyStreak
    , SomeCol SettingsTBowHits, SomeCol SettingsTBowShots, SomeCol SettingsTAccuracy]

getSettingsByDiscord :: (MonadIOReader m r, Has Connection r) => UserId -> m Settings
getSettingsByDiscord discord = fromMaybe defSettings <$> queryOnlyLogT selectByPrimaryQuery (SettingsTPrimary discord)

setSettingsByDiscord :: (MonadIOReader m r, Has Connection r) => UserId -> Settings -> m Bool
setSettingsByDiscord discord settings = (>0) <$> executeLogT insertQuery' (SettingsTPrimary discord, settings)

defSettings :: Settings
defSettings = Settings
  { sWins = Yes
  , sLosses = Yes
  , sWLR = Always
  , sWinsUntil = Always
  , sBestStreak = Always
  , sCurrentStreak = Always
  , sBestDailyStreak = Never
  , sBowHits = No
  , sBowShots = No
  , sAccuracy = Never
  }

allSettings :: Settings
allSettings = Settings
  { sWins = Yes
  , sLosses = Yes
  , sWLR = Always
  , sWinsUntil = Always
  , sBestStreak = Always
  , sCurrentStreak = Always
  , sBestDailyStreak = Always
  , sBowHits = Yes
  , sBowShots = Yes
  , sAccuracy = Always
  }

data SettingsSource = DefSettings | AllSettings | UserSettings

getSettingsFromSource :: (MonadIOReader m r, Has Connection r) => SettingsSource -> UserId -> m Settings
getSettingsFromSource DefSettings _ = return defSettings
getSettingsFromSource AllSettings _ = return allSettings
getSettingsFromSource UserSettings discord = getSettingsByDiscord discord

data SingleSetting = SingleSettingBin (Settings -> SettingBin) (Settings -> SettingBin -> Settings) | SingleSettingTer (Settings -> SettingTer) (Settings -> SettingTer -> Settings)

getSingleSettingByName :: Text -> Maybe SingleSetting
getSingleSettingByName "wins" = Just $ SingleSettingBin sWins $ \s b -> s { sWins = b }
getSingleSettingByName "losses" = Just $ SingleSettingBin sLosses $ \s b -> s { sLosses = b }
getSingleSettingByName "wlr" = Just $ SingleSettingTer sWLR $ \s b -> s { sWLR = b }
getSingleSettingByName "winsuntil" = Just $ SingleSettingTer sWinsUntil $ \s b -> s { sWinsUntil = b }
getSingleSettingByName "beststreak" = Just $ SingleSettingTer sBestStreak $ \s b -> s { sBestStreak = b }
getSingleSettingByName "currentstreak" = Just $ SingleSettingTer sCurrentStreak $ \s b -> s { sCurrentStreak = b }
getSingleSettingByName "bestdailystreak" = Just $ SingleSettingTer sBestDailyStreak $ \s b -> s { sBestDailyStreak = b }
getSingleSettingByName "bowhits" = Just $ SingleSettingBin sBowHits $ \s b -> s { sBowHits = b }
getSingleSettingByName "bowshots" = Just $ SingleSettingBin sBowShots $ \s b -> s { sBowShots = b }
getSingleSettingByName "accuracy" = Just $ SingleSettingTer sAccuracy $ \s b -> s { sAccuracy = b }
getSingleSettingByName _ = Nothing