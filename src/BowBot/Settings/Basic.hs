{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.Settings.Basic where

import BowBot.BotData.Cached
import Discord.Types (UserId)
import BowBot.Discord.Orphans ()
import qualified Data.HashMap.Strict as HM
import BowBot.DB.Basic
import BowBot.Utils
import Database.MySQL.Simple (Param, Result, ToField(..), FromField(..))
import qualified Database.MySQL.Base.Types as T

data SettingBin = Yes | No deriving (Show, Eq, Ord, Enum)

instance Param SettingBin
instance Result SettingBin

instance ToField SettingBin where
  toField Yes = "yes"
  toField No = "no"

instance FromField SettingBin where
  fromField = ([T.Enum, T.String], \case
    "yes" -> Right Yes
    "no" -> Right No
    _ -> Left "Wrong permission level")

data SettingTer = Never | WhenSensible | Always deriving (Show, Eq, Ord, Enum)

instance Param SettingTer
instance Result SettingTer

instance ToField SettingTer where
  toField Always = "always"
  toField Never = "never"
  toField WhenSensible = "sensibly"

instance FromField SettingTer where
  fromField = ([T.Enum, T.String], \case
    "always" -> Right Always
    "never" -> Right Never
    "sensibly" -> Right WhenSensible
    _ -> Left "Wrong permission level")

data Settings = Settings
  { sWins :: !SettingBin, sLosses :: !SettingBin, sWLR :: !SettingTer, sWinsUntil :: !SettingTer
  , sBestStreak :: !SettingTer, sCurrentStreak :: !SettingTer, sBestDailyStreak :: !SettingTer
  , sBowHits :: !SettingBin, sBowShots :: !SettingBin, sAccuracy :: !SettingTer
  } deriving (Show, Eq)

onlyIfBin :: SettingBin -> a -> Maybe a
onlyIfBin Yes a = Just a
onlyIfBin No _ = Nothing

onlyIfTer :: SettingTer -> Bool -> a -> Maybe a
onlyIfTer Always _ a = Just a
onlyIfTer Never _ _ = Nothing
onlyIfTer WhenSensible True a = Just a
onlyIfTer WhenSensible False _ = Nothing

instance Cached Settings where
  type CacheIndex Settings = UserId
  refreshCache = do
    cache <- getCache
    res :: [(UserId, SettingBin, SettingBin, SettingTer, SettingTer, SettingTer, SettingTer, SettingTer, SettingBin, SettingBin, SettingTer)] <-
      queryLog "SELECT `discord`, `wins`, `losses`, `wlr`, `winsUntil`, `bestStreak`, `currentStreak`, `bestDailyStreak`, `bowHits`, `bowShots`, `accuracy` FROM `settings`" ()
    let newValues = HM.fromList $ flip fmap res $ \(discord, sWins, sLosses, sWLR, sWinsUntil, sBestStreak, sCurrentStreak, sBestDailyStreak, sBowHits, sBowShots, sAccuracy) -> (discord, Settings {..})
    liftIO $ atomically $ writeTVar cache newValues

instance CachedStorable Settings where
  storeInCacheIndexed accs = do
    cacheMap <- getCacheMap
    let toQueryParams (d, set@Settings {..}) = if Just set == cacheMap HM.!? d then Nothing else Just (toInteger d, sWins, sLosses, sWLR, sWinsUntil, sBestStreak, sCurrentStreak, sBestDailyStreak, sBowHits, sBowShots, sAccuracy)
    let queryParams = mapMaybe toQueryParams accs
    success <- liftIO $ withDB $ \conn -> (>0) <$> executeManyLog' conn "INSERT INTO `settings` (`discord`, `wins`, `losses`, `wlr`, `winsUntil`, `bestStreak`, `currentStreak`, `bestDailyStreak`, `bowHits`, `bowShots`, `accuracy`) VALUES (?,?,?,?,?,?,?,?,?,?,?) ON DUPLICATE KEY UPDATE `wins`=VALUES(`wins`), `losses`=VALUES(`losses`), `wlr`=VALUES(`wlr`), `winsUntil`=VALUES(`winsUntil`), `bestStreak`=VALUES(`bestStreak`), `currentStreak`=VALUES(`currentStreak`), `bestDailyStreak`=VALUES(`bestDailyStreak`), `bowHits`=VALUES(`bowHits`), `bowShots`=VALUES(`bowShots`), `accuracy`=VALUES(`accuracy`)"  queryParams
    when success $ do
      cache <- getCache
      liftIO $ atomically $ modifyTVar cache (insertMany accs)
    return success

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

getSettingsFromSource :: (MonadIOBotData m d r, HasCache Settings d) => SettingsSource -> UserId -> m Settings
getSettingsFromSource DefSettings _ = return defSettings
getSettingsFromSource AllSettings _ = return allSettings
getSettingsFromSource UserSettings user = fromMaybe defSettings <$> getFromCache user

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