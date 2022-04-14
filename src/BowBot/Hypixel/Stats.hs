{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.Hypixel.Stats where

import BowBot.Settings.Basic
import BowBot.Network.Class (MonadNetwork)
import BowBot.Minecraft.Basic (UUID)
import BowBot.Hypixel.Basic
import Data.Aeson
import BowBot.Utils
import Data.Maybe (catMaybes, isJust, isNothing)
import BowBot.Hypixel.Division
import Data.Ratio ((%))

data HypixelBowStats = HypixelBowStats
  { bowWins :: Integer,
    bowLosses :: Integer,
    bestWinstreak :: Maybe Integer,
    currentWinstreak :: Maybe Integer,
    bestDailyWinstreak :: Maybe Integer,
    bowHits :: Integer,
    bowShots :: Integer
  } deriving (Show)


requestHypixelBowStats :: (MonadNetwork m) => UUID -> m (Maybe HypixelBowStats)
requestHypixelBowStats uuid = hypixelWithPlayerData uuid $ \o -> do
    pl <- o .: "player"
    stats <- pl .: "stats"
    duelsStats <- stats .:? "Duels"
    bowWins <- fromMaybe 0 <$> for duelsStats (\x -> x .:? "bow_duel_wins" .!= 0)
    bowLosses <- fromMaybe 0 <$> for duelsStats (\x -> x .:? "bow_duel_losses" .!= 0)
    bestWinstreak <- (\a -> if a == Just 0 then Nothing else a) <$> for duelsStats (\x -> x .:? "best_winstreak_mode_bow_duel" .!= 0)
    currentWinstreak <- (\a -> if isNothing bestWinstreak then Nothing else a) <$> for duelsStats (\x -> x .:? "current_bow_winstreak" .!= 0)
    bestDailyWinstreak <- (\a -> if isNothing bestWinstreak then Nothing else a) <$> for duelsStats (\x -> x .:? "duels_winstreak_best_bow_duel" .!= 0)
    bowHits <- fromMaybe 0 <$> for duelsStats (\x -> x .:? "bow_duel_bow_hits" .!= 0)
    bowShots <- fromMaybe 0 <$> for duelsStats (\x -> x .:? "bow_duel_bow_shots" .!= 0)
    return HypixelBowStats {..}


showHypixelBowStats :: Settings -> HypixelBowStats -> String
showHypixelBowStats Settings {..} HypixelBowStats {..} = unlines $ catMaybes
  [ onlyIf sWins
  $ " - *Bow Duels Wins:* **"
  ++ show bowWins
  ++ "**" ++ maybe "" (\x -> " (**Bow " ++ divisionRankName x ++ "**)") (divisionRankFromWins bowWins)
  , onlyIf sLosses
  $ " - *Bow Duels Losses:* **"
  ++ show bowLosses
  ++ "**"
  , onlyIf (sense sWLR (bowWins + bowLosses /= 0))
  $ " - *Bow Duels Win/Loss Ratio:* **"
  ++ winLossRatio
  ++ "**"
  , onlyIf (sense sWinsUntil (bowLosses /= 0))
  $ " - *Bow Duels Wins until "
  ++ nextWinLossRatio
  ++ " WLR:* **"
  ++ winsRemaining
  ++ "**"
  , onlyIf (sense sBestStreak (isJust bestWinstreak))
  $ " - *Best Bow Duels Winstreak:* **"
  ++ maybe "API DISABLED" show bestWinstreak
  ++ "**"
  , onlyIf (sense sCurrentStreak (isJust currentWinstreak))
  $ " - *Current Bow Duels Winstreak:* **"
  ++ maybe "API DISABLED" show currentWinstreak
  ++ "**"
  , onlyIf (sense sBestDailyStreak (isJust bestDailyWinstreak))
  $ " - *Best Daily Bow Duels Winstreak(?):* **"
  ++ maybe "API DISABLED" show bestDailyWinstreak
  ++ "**"
  , onlyIf sBowHits
  $ " - *Bow Hits in Bow Duels:* **"
  ++ show bowHits
  ++ "**"
  , onlyIf sBowShots
  $ " - *Bow Shots in Bow Duels:* **"
  ++ show bowShots
  ++ "**"
  , onlyIf (sense sAccuracy (bowShots /= 0))
  $ " - *Bow Accuracy:* **"
  ++ accuracy
  ++ "**"
  ]
  where
    sense Always _ = True
    sense Never _ = False
    sense WhenSensible x = x
    onlyIf True a = Just a
    onlyIf False _ = Nothing
    winLossRatio = showWLR bowWins bowLosses
    nextWinLossRatio
      | bowLosses == 0 = "∞"
      | otherwise = show $ (bowWins `div` bowLosses) + 1
    winsRemaining
      | bowWins == 0, bowLosses == 0 = "1"
      | bowLosses == 0 = "N/A"
      | otherwise = show (bowLosses - (bowWins `mod` bowLosses))
    accuracy
      | bowShots == 0 = "N/A"
      | otherwise = show (round ((bowHits*100) % bowShots) :: Integer) ++ "%"