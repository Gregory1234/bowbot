{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.Settings.Basic where

import BowBot.BotData.Cached
import Discord.Types (UserId)
import BowBot.Discord.DiscordNFData ()
import qualified Data.HashMap.Strict as HM
import BowBot.DB.Basic
import BowBot.Utils

data BoolSense = Never | WhenSensible | Always deriving (Show, Eq, Ord, Enum)

data Settings = Settings
  { sWins :: !Bool, sLosses :: !Bool, sWLR :: !BoolSense, sWinsUntil :: !BoolSense
  , sBestStreak :: !BoolSense, sCurrentStreak :: !BoolSense, sBestDailyStreak :: !BoolSense
  , sBowHits :: !Bool, sBowShots :: !Bool, sAccuracy :: !BoolSense
  } deriving (Show, Eq)

parseBool :: Text -> Maybe Bool
parseBool "yes" = Just True
parseBool "no" = Just False
parseBool _ = Nothing

parseSense :: Text -> Maybe BoolSense
parseSense "always" = Just Always
parseSense "never" = Just Never
parseSense "sensibly" = Just WhenSensible
parseSense _ = Nothing

stringBool :: Bool -> Text
stringBool True = "yes"
stringBool False = "no"

stringSense :: BoolSense -> Text
stringSense Always = "always"
stringSense Never = "never"
stringSense WhenSensible = "sensibly"

instance Cached Settings where
  type CacheIndex Settings = UserId
  refreshCache = do
    cache <- getCache
    res :: [(Integer, Text, Text, Text, Text, Text, Text, Text, Text, Text, Text)] <-
      queryLog "SELECT `discord`, `wins`, `losses`, `wlr`, `winsUntil`, `bestStreak`, `currentStreak`, `bestDailyStreak`, `bowHits`, `bowShots`, `accuracy` FROM `settings`" ()
    let newValues = HM.fromList $ flip fmap res $ \case
          (fromInteger -> discord,
            parseBool -> Just sWins, parseBool -> Just sLosses, parseSense -> Just sWLR, parseSense -> Just sWinsUntil,
            parseSense -> Just sBestStreak, parseSense -> Just sCurrentStreak, parseSense -> Just sBestDailyStreak,
            parseBool -> Just sBowHits, parseBool -> Just sBowShots, parseSense -> Just sAccuracy) -> (discord, Settings {..})
          (fromInteger -> discord, _, _, _, _, _, _, _, _, _, _) -> (discord, defSettings)
    liftIO $ atomically $ writeTVar cache newValues

instance CachedStorable Settings where
  storeInCacheIndexed accs = do
    cacheMap <- getCacheMap
    let toQueryParams (d, set@Settings {..}) = if Just set == cacheMap HM.!? d then Nothing else Just (toInteger d, stringBool sWins, stringBool sLosses, stringSense sWLR, stringSense sWinsUntil, stringSense sBestStreak, stringSense sCurrentStreak, stringSense sBestDailyStreak, stringBool sBowHits, stringBool sBowShots, stringSense sAccuracy)
    let queryParams = mapMaybe toQueryParams accs
    success <- liftIO $ withDB $ \conn -> (>0) <$> executeManyLog' conn "INSERT INTO `settings` (`discord`, `wins`, `losses`, `wlr`, `winsUntil`, `bestStreak`, `currentStreak`, `bestDailyStreak`, `bowHits`, `bowShots`, `accuracy`) VALUES (?,?,?,?,?,?,?,?,?,?,?) ON DUPLICATE KEY UPDATE `wins`=VALUES(`wins`), `losses`=VALUES(`losses`), `wlr`=VALUES(`wlr`), `winsUntil`=VALUES(`winsUntil`), `bestStreak`=VALUES(`bestStreak`), `currentStreak`=VALUES(`currentStreak`), `bestDailyStreak`=VALUES(`bestDailyStreak`), `bowHits`=VALUES(`bowHits`), `bowShots`=VALUES(`bowShots`), `accuracy`=VALUES(`accuracy`)"  queryParams
    when success $ do
      cache <- getCache
      liftIO $ atomically $ modifyTVar cache (insertMany accs)
    return success

defSettings :: Settings
defSettings = Settings
  { sWins = True
  , sLosses = True
  , sWLR = Always
  , sWinsUntil = Always
  , sBestStreak = Always
  , sCurrentStreak = Always
  , sBestDailyStreak = Never
  , sBowHits = False
  , sBowShots = False
  , sAccuracy = Never
  }

allSettings :: Settings
allSettings = Settings
  { sWins = True
  , sLosses = True
  , sWLR = Always
  , sWinsUntil = Always
  , sBestStreak = Always
  , sCurrentStreak = Always
  , sBestDailyStreak = Always
  , sBowHits = True
  , sBowShots = True
  , sAccuracy = Always
  }

data SettingsSource = DefSettings | AllSettings | UserSettings

getSettingsFromSource :: (MonadIOBotData m d r, HasCache Settings d) => SettingsSource -> UserId -> m Settings
getSettingsFromSource DefSettings _ = return defSettings
getSettingsFromSource AllSettings _ = return allSettings
getSettingsFromSource UserSettings user = fromMaybe defSettings <$> getFromCache user

data SingleSetting = SingleSettingBool (Settings -> Bool) (Settings -> Bool -> Settings) | SingleSettingSense (Settings -> BoolSense) (Settings -> BoolSense -> Settings)

getSingleSettingByName :: Text -> Maybe SingleSetting
getSingleSettingByName "wins" = Just $ SingleSettingBool sWins $ \s b -> s { sWins = b }
getSingleSettingByName "losses" = Just $ SingleSettingBool sLosses $ \s b -> s { sLosses = b }
getSingleSettingByName "wlr" = Just $ SingleSettingSense sWLR $ \s b -> s { sWLR = b }
getSingleSettingByName "winsuntil" = Just $ SingleSettingSense sWinsUntil $ \s b -> s { sWinsUntil = b }
getSingleSettingByName "beststreak" = Just $ SingleSettingSense sBestStreak $ \s b -> s { sBestStreak = b }
getSingleSettingByName "currentstreak" = Just $ SingleSettingSense sCurrentStreak $ \s b -> s { sCurrentStreak = b }
getSingleSettingByName "bestdailystreak" = Just $ SingleSettingSense sBestDailyStreak $ \s b -> s { sBestDailyStreak = b }
getSingleSettingByName "bowhits" = Just $ SingleSettingBool sBowHits $ \s b -> s { sBowHits = b }
getSingleSettingByName "bowshots" = Just $ SingleSettingBool sBowShots $ \s b -> s { sBowShots = b }
getSingleSettingByName "accuracy" = Just $ SingleSettingSense sAccuracy $ \s b -> s { sAccuracy = b }
getSingleSettingByName _ = Nothing