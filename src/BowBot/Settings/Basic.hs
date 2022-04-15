{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.Settings.Basic where

import BowBot.BotData.Cached
import Discord.Types (UserId)
import BowBot.Discord.DiscordNFData ()
import qualified Data.HashMap.Strict as HM
import Data.Proxy
import BowBot.DB.Basic
import BowBot.Utils

data BoolSense = Never | WhenSensible | Always deriving (Show, Eq, Ord, Enum)

data Settings = Settings
  { sWins :: Bool, sLosses :: Bool, sWLR :: BoolSense, sWinsUntil :: BoolSense
  , sBestStreak :: BoolSense, sCurrentStreak :: BoolSense, sBestDailyStreak :: BoolSense
  , sBowHits :: Bool, sBowShots :: Bool, sAccuracy :: BoolSense
  } deriving (Show, Eq)

parseBool :: String -> Maybe Bool
parseBool "yes" = Just True
parseBool "no" = Just False
parseBool _ = Nothing

parseSense :: String -> Maybe BoolSense
parseSense "always" = Just Always
parseSense "never" = Just Never
parseSense "sensibly" = Just WhenSensible
parseSense _ = Nothing

stringBool :: Bool -> String
stringBool True = "yes"
stringBool False = "no"

stringSense :: BoolSense -> String
stringSense Always = "always"
stringSense Never = "never"
stringSense WhenSensible = "sensibly"

instance Cached Settings where
  type CacheIndex Settings = UserId
  refreshCache conn _ = do
    cache <- getCache (Proxy @Settings)
    res :: [(Integer, String, String, String, String, String, String, String, String, String, String)] <-
      queryLog conn "SELECT `discord`, `wins`, `losses`, `wlr`, `winsUntil`, `bestStreak`, `currentStreak`, `bestDailyStreak`, `bowHits`, `bowShots`, `accuracy` FROM `settingsDEV`" ()
    let newValues = HM.fromList $ flip fmap res $ \case
          (fromInteger -> discord,
            parseBool -> Just sWins, parseBool -> Just sLosses, parseSense -> Just sWLR, parseSense -> Just sWinsUntil,
            parseSense -> Just sBestStreak, parseSense -> Just sCurrentStreak, parseSense -> Just sBestDailyStreak,
            parseBool -> Just sBowHits, parseBool -> Just sBowShots, parseSense -> Just sAccuracy) -> (discord, Settings {..})
          (fromInteger -> discord, _, _, _, _, _, _, _, _, _, _) -> (discord, defSettings)
    liftIO $ atomically $ writeTVar cache newValues
  storeInCacheIndexed accs = do
    let toQueryParams (d, Settings {..}) = (toInteger d, stringBool sWins, stringBool sLosses, stringSense sWLR, stringSense sWinsUntil, stringSense sBestStreak, stringSense sCurrentStreak, stringSense sBestDailyStreak, stringBool sBowHits, stringBool sBowShots, stringSense sAccuracy)
    success <- liftIO $ withDB $ \conn -> (== fromIntegral (length accs)) <$> executeManyLog conn "INSERT INTO `minecraftDEV` (`discord`, `wins`, `losses`, `wlr`, `winsUntil`, `bestStreak`, `currentStreak`, `bestDailyStreak`, `bowHits`, `bowShots`, `accuracy`) VALUES (?,?,?,?,?,?,?,?,?,?,?) ON DUPLICATE KEY UPDATE `wins`=VALUES(`wins`), `losses`=VALUES(`losses`), `wlr`=VALUES(`wlr`), `winsUntil`=VALUES(`winsUntil`), `bestStreak`=VALUES(`bestStreak`), `currentStreak`=VALUES(`currentStreak`), `bestDailyStreak`=VALUES(`bestDailyStreak`), `bowHits`=VALUES(`bowHits`), `bowShots`=VALUES(`bowShots`), `accuracy`=VALUES(`accuracy`)" (map toQueryParams accs)
    when success $ do
      cache <- getCache (Proxy @Settings)
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

getSettingsFromSource :: MonadCache Settings m => SettingsSource -> UserId -> m Settings
getSettingsFromSource DefSettings _ = return defSettings
getSettingsFromSource AllSettings _ = return allSettings
getSettingsFromSource UserSettings user = fromMaybe defSettings <$> getFromCache (Proxy @Settings) user