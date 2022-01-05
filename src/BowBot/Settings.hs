{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module BowBot.Settings(
  module BowBot.Settings, module BowBot.BotData.Core
) where

import Discord.Types
import Data.Map (Map, fromList)
import BowBot.DB
import BowBot.BotData.Core

parseBool :: String -> Maybe Bool
parseBool "yes" = Just True
parseBool "no" = Just False
parseBool _ = Nothing

parseSense :: String -> Maybe BoolSense
parseSense "always" = Just Always
parseSense "never" = Just Never
parseSense "sensibly" = Just WhenSensible
parseSense _ = Nothing

getSettings :: DBMonad m => m (Map UserId Settings)
getSettings = do
  res :: [(Integer, String, String, String, String, String, String, String, String, String, String)] <- 
    hQueryLog "SELECT `discord`, `wins`, `losses`, `wlr`, `winsUntil`, `bestStreak`, `currentStreak`, `bestDailyStreak`, `bowHits`, `bowShots`, `accuracy` FROM `settingsDEV`" ()
  return $ fromList $ flip fmap res $ \case
    (fromInteger -> discord, 
      parseBool -> Just sWins, parseBool -> Just sLosses, parseSense -> Just sWLR, parseSense -> Just sWinsUntil, 
      parseBool -> Just sBestStreak, parseBool -> Just sCurrentStreak, parseBool -> Just sBestDailyStreak, 
      parseBool -> Just sBowHits, parseBool -> Just sBowShots, parseSense -> Just sAccuracy) -> (discord, Settings {..})
    (fromInteger -> discord, _, _, _, _, _, _, _, _, _, _) -> (discord, defSettings)

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