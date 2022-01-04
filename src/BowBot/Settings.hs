{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module BowBot.Settings(
  module BowBot.Settings, module BowBot.BotData.Core
) where

import Discord.Types
import Data.Map (Map, fromList)
import BowBot.API
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

getSettings :: APIMonad m => m (Maybe (Map UserId Settings))
getSettings = do
  res <- hSendDB "discord/settings/all.php" []
  decodeParse res $ \o -> do
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