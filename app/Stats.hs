{-# LANGUAGE RecordWildCards #-}

module Stats where

import Data.Maybe (catMaybes)
import Text.Printf (printf)
import Data.Ratio ((%))
import Utils (discordEscape)


data BoolSense = Never | WhenSensible | Always deriving (Show, Eq, Ord, Enum)

data Stats = Stats
  { playerName :: String,
    bowWins :: Integer,
    bowLosses :: Integer,
    bestWinstreak :: Integer,
    currentWinstreak :: Integer,
    bestDailyWinstreak :: Integer,
    bowHits :: Integer,
    bowShots :: Integer
  }
  deriving (Show)

data StatsSettings = StatsSettings
  { sWins :: Bool
  , sLosses :: Bool
  , sWLR :: BoolSense
  , sWinsUntil :: BoolSense
  , sBestStreak :: Bool
  , sCurrentStreak :: Bool
  , sBestDailyStreak :: Bool
  , sBowHits :: Bool
  , sBowShots :: Bool
  , sAccuracy :: BoolSense
  } deriving (Eq, Show)

defSettings :: StatsSettings
defSettings = StatsSettings
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

allSettings :: StatsSettings
allSettings = StatsSettings
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

addOldName :: String -> Stats -> Stats
addOldName n st@Stats {..} = st {playerName = n ++ " (" ++ playerName ++ ")" } 

showStats :: StatsSettings -> Stats -> String
showStats StatsSettings {..} Stats {..} = unlines $ catMaybes
  [ onlyIf True
  $ "**" ++ discordEscape playerName ++ ":**"
  , onlyIf sWins
  $ " - *Bow Duels Wins:* **"
  ++ show bowWins
  ++ "**"
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
  , onlyIf sBestStreak
  $ " - *Best Bow Duels Winstreak:* **"
  ++ show bestWinstreak
  ++ "**"
  , onlyIf sCurrentStreak
  $ " - *Current Bow Duels Winstreak:* **"
  ++ show currentWinstreak
  ++ "**"
  , onlyIf sBestDailyStreak
  $ " - *Best Daily Bow Duels Winstreak(?):* **"
  ++ show bestDailyWinstreak
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
    winLossRatio
      | bowWins == 0, bowLosses == 0 = "NaN"
      | bowLosses == 0 = "∞"
      | otherwise = printf "%.04f" (fromRational (bowWins % bowLosses) :: Double)
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