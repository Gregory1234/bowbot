{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module BowBot.Settings where

import Discord.Types
import Data.Map (Map, fromList)
import Network.HTTP.Conduit (Manager)
import BowBot.API
import Data.Aeson.Types (parseMaybe, (.:))
import Data.Aeson (decode)
import Data.Traversable (for)
import Text.Read (readMaybe)


data BoolSense = Never | WhenSensible | Always deriving (Show, Eq, Ord, Enum)

data Settings = Settings
  { sWins :: Bool, sLosses :: Bool, sWLR :: BoolSense, sWinsUntil :: BoolSense
  , sBestStreak :: Bool, sCurrentStreak :: Bool, sBestDailyStreak :: Bool
  , sBowHits :: Bool, sBowShots :: Bool, sAccuracy :: BoolSense
  } deriving (Show, Eq)

parseBool :: String -> Maybe Bool
parseBool "yes" = Just True
parseBool "no" = Just False
parseBool _ = Nothing

parseSense :: String -> Maybe BoolSense
parseSense "always" = Just Always
parseSense "never" = Just Always
parseSense "sensibly" = Just WhenSensible
parseSense _ = Nothing

getSettings :: Manager -> IO (Maybe (Map UserId Settings))
getSettings manager = do
  res <- sendDB manager "discord/settings/all.php" []
  let parser = parseMaybe $ \o -> do
        dt <- o .: "data"
        fmap fromList $ for dt $ \settings -> do
          (readMaybe -> Just discord) <- settings .: "discord"
          (parseBool -> Just sWins) <- settings .: "wins"
          (parseBool -> Just sLosses) <- settings .: "losses"
          (parseSense -> Just sWLR) <- settings .: "wlr"
          (parseSense -> Just sWinsUntil) <- settings .: "winsUntil"
          (parseBool -> Just sBestStreak) <- settings .: "bestStreak"
          (parseBool -> Just sCurrentStreak) <- settings .: "currentStreak"
          (parseBool -> Just sBestDailyStreak) <- settings .: "bestDailyStreak"
          (parseBool -> Just sBowHits) <- settings .: "bowHits"
          (parseBool -> Just sBowShots) <- settings .: "bowShots"
          (parseSense -> Just sAccuracy) <- settings .: "accuracy"
          pure (discord, Settings {..})
  return $ decode res >>= parser

updateSettings :: Manager -> UserId -> String -> String -> IO (Maybe String)
updateSettings = undefined

defSettings :: Settings
defSettings = Settings
  { sWins = True
  , sLosses = True
  , sWLR = Always
  , sWinsUntil = Always
  , sBestStreak = True
  , sCurrentStreak = True
  , sBestDailyStreak = False
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
  , sBestStreak = True
  , sCurrentStreak = True
  , sBestDailyStreak = True
  , sBowHits = True
  , sBowShots = True
  , sAccuracy = Always
  }