{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module BowBot.Settings.Basic where

import Discord.Types (UserId)
import BowBot.Discord.Orphans ()
import BowBot.DB.Basic
import BowBot.Utils

data SettingBin = Yes | No
  deriving (Show, Eq, Ord, Enum)
  deriving (ToMysqlSimple, FromMysqlSimple, ToMysql, FromMysql) via (EnumValue SettingBin)

instance MysqlEnum SettingBin where
  toMysqlEnum Yes = "yes"
  toMysqlEnum No = "no"
  fromMysqlEnum "yes" = Yes
  fromMysqlEnum "no" = No
  fromMysqlEnum _ = error "Wrong permission level"

data SettingTer = Never | WhenSensible | Always
  deriving (Show, Eq, Ord, Enum)
  deriving (ToMysqlSimple, FromMysqlSimple, ToMysql, FromMysql) via (EnumValue SettingTer)

instance MysqlEnum SettingTer where
  toMysqlEnum Always = "always"
  toMysqlEnum Never = "never"
  toMysqlEnum WhenSensible = "sensibly"
  fromMysqlEnum "always" = Always
  fromMysqlEnum "never" = Never
  fromMysqlEnum "sensibly" = WhenSensible
  fromMysqlEnum _ = error "Wrong permission level"

onlyIfBin :: SettingBin -> a -> Maybe a
onlyIfBin Yes a = Just a
onlyIfBin No _ = Nothing

onlyIfTer :: SettingTer -> Bool -> a -> Maybe a
onlyIfTer Always _ a = Just a
onlyIfTer Never _ _ = Nothing
onlyIfTer WhenSensible True a = Just a
onlyIfTer WhenSensible False _ = Nothing

data Settings = Settings
  { sWins :: !SettingBin, sLosses :: !SettingBin, sWLR :: !SettingTer, sWinsUntil :: !SettingTer
  , sBestStreak :: !SettingTer, sCurrentStreak :: !SettingTer, sBestDailyStreak :: !SettingTer
  , sBowHits :: !SettingBin, sBowShots :: !SettingBin, sAccuracy :: !SettingTer
  } deriving stock (Show, Eq, Generic)
    deriving (ToMysql, FromMysql) via (Generically Settings)

$(pure [])

getSettingsByDiscord :: (MonadIOReader m r, Has SafeMysqlConn r) => UserId -> m Settings
getSettingsByDiscord discord = fromMaybe defSettings <$> queryOnlyLog [mysql|SELECT Settings FROM `settings` WHERE `discord_id` = discord|]

setSettingsByDiscord :: (MonadIOReader m r, Has SafeMysqlConn r) => UserId -> Settings -> m Bool
setSettingsByDiscord discord settings = (>0) <$> executeLog [mysql|INSERT INTO `settings`(`discord_id`, Settings) VALUES (discord, settings)|]

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

getSettingsFromSource :: (MonadIOReader m r, Has SafeMysqlConn r) => SettingsSource -> UserId -> m Settings
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