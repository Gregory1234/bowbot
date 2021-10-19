{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module BowBot.Stats.HypixelBow where

import BowBot.Utils
import Data.Proxy
import BowBot.Stats
import BowBot.API
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ratio ((%))
import Data.Aeson (decode, (.:))
import System.Environment.Blank (getEnv)
import Data.Aeson.Types (parseMaybe, (.!=), (.:?), object, (.=), parseEither)
import Data.Traversable (for)
import Data.Map (fromList, toList)
import Data.Text (pack)
import Text.Read (readMaybe)

data HypixelBowStats = HypixelBowStats
  { bowWins :: Integer,
    bowLosses :: Integer,
    bestWinstreak :: Integer,
    currentWinstreak :: Integer,
    bestDailyWinstreak :: Integer,
    bowHits :: Integer,
    bowShots :: Integer
  } deriving (Show)

instance StatType HypixelBowStats where
  data Settings HypixelBowStats = HypixelBowSettings
    { sWins :: Bool, sLosses :: Bool, sWLR :: BoolSense, sWinsUntil :: BoolSense
    , sBestStreak :: Bool, sCurrentStreak :: Bool, sBestDailyStreak :: Bool
    , sBowHits :: Bool, sBowShots :: Bool, sAccuracy :: BoolSense
    } deriving (Eq, Show)
  data Leaderboards HypixelBowStats = HypixelBowLeaderboards
    { bowLbWins :: Integer, bowLbLosses :: Integer, bowLbWinstreak :: Integer } deriving (Show)
  data SettingsUpdate HypixelBowStats
    = UpdateWins Bool | UpdateLosses Bool | UpdateWLR BoolSense | UpdateWinsUntil BoolSense
    | UpdateBestStreak Bool | UpdateCurrentStreak Bool | UpdateBestDailyStreak Bool
    | UpdateBowHist Bool | UpdateBowShots Bool | UpdateAccuracy BoolSense
    deriving (Eq, Show)
  requestStats Proxy manager uuid = do
    apiKey <- fromMaybe "" <$> getEnv "HYPIXEL_API"
    let url = "https://api.hypixel.net/player?key=" ++ apiKey ++ "&uuid=" ++ uuid
    let cleanUrl = "https://api.hypixel.net/player?key=[REDACTED]&uuid=" ++ uuid
    res <- sendRequestTo manager url cleanUrl
    let parser = parseMaybe $ \o -> do
          pl <- o .: "player"
          stats <- pl .: "stats"
          duelsStats <- stats .:? "Duels"
          bowWins <- fromMaybe 0 <$> for duelsStats (\x -> x .:? "bow_duel_wins" .!= 0)
          bowLosses <- fromMaybe 0 <$> for duelsStats (\x -> x .:? "bow_duel_losses" .!= 0)
          currentWinstreak <- fromMaybe 0 <$> for duelsStats (\x -> x .:? "current_bow_winstreak" .!= 0)
          bestWinstreak <- fromMaybe 0 <$> for duelsStats (\x -> x .:? "best_bow_winstreak" .!= 0)
          bestDailyWinstreak <- fromMaybe 0 <$> for duelsStats (\x -> x .:? "duels_winstreak_best_bow_duel" .!= 0)
          bowHits <- fromMaybe 0 <$> for duelsStats (\x -> x .:? "bow_duel_bow_hits" .!= 0)
          bowShots <- fromMaybe 0 <$> for duelsStats (\x -> x .:? "bow_duel_bow_shots" .!= 0)
          return HypixelBowStats {..}
    return $ decode res >>= parser

  showStats HypixelBowSettings {..} HypixelBowStats {..} = unlines $ catMaybes
    [ onlyIf sWins
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

  statsNotable HypixelBowStats {..} = bowWins >= 50

  toLeaderboard HypixelBowStats {..} = HypixelBowLeaderboards 
    { bowLbWins = bowWins, bowLbLosses = bowLosses, bowLbWinstreak = bestWinstreak }
  getLeaderboard Proxy manager = do
    res <- sendDB manager "stats/hypixel/leaderboard.php" []
    let parser = parseMaybe $ \o -> do
            dt <- o .: "data"
            for dt $ \s -> do
              uuid <- s .: "uuid"
              (readMaybe -> Just bowLbWins) <- s .: "bowWins"
              (readMaybe -> Just bowLbLosses) <- s .: "bowLosses"
              (readMaybe -> Just bowLbWinstreak) <- s .: "bowWinstreak"
              return (uuid, HypixelBowLeaderboards {..})
    return . fmap fromList $ decode res >>= parser
  updateLeaderboard manager lb = sendPostDB manager "stats/hypixel/update.php" (object $ helper <$> toList lb)
    where
      helper (uuid, HypixelBowLeaderboards {..}) = pack uuid .= object ["bowWins" .= bowLbWins, "bowLosses" .= bowLbLosses, "bowWinstreak" .= bowLbWinstreak]
  defSettings Proxy = HypixelBowSettings
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
  
  allSettings Proxy = HypixelBowSettings
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
  
  getSettings Proxy manager = return Nothing -- TODO
  updateSettings manager did setupt = return () -- TODO

hypixelBowWinsLeaderboard :: Leaderboards HypixelBowStats -> Maybe (Integer, String)
hypixelBowWinsLeaderboard HypixelBowLeaderboards {..} | bowLbWins >= 500 = Just (bowLbWins, show bowLbWins)
hypixelBowWinsLeaderboard _ = Nothing

hypixelBowLossesLeaderboard :: Leaderboards HypixelBowStats -> Maybe (Integer, String)
hypixelBowLossesLeaderboard HypixelBowLeaderboards {..} | bowLbWins >= 500 = Just (bowLbLosses, show bowLbLosses)
hypixelBowLossesLeaderboard _ = Nothing

hypixelBowWinstreakLeaderboard :: Leaderboards HypixelBowStats -> Maybe (Integer, String)
hypixelBowWinstreakLeaderboard HypixelBowLeaderboards {..} | bowLbWinstreak >= 50 = Just (bowLbWinstreak, show bowLbWinstreak)
hypixelBowWinstreakLeaderboard _ = Nothing

hypixelBowWLRLeaderboard :: Leaderboards HypixelBowStats -> Maybe (Integer, String)
hypixelBowWLRLeaderboard HypixelBowLeaderboards {..} | bowLbWins >= bowLbLosses = Just (if bowLbWins == 0 then bowLbWins*1000000 else (bowLbWins*10000) `div` bowLbLosses, showWLR bowLbWins bowLbLosses)
hypixelBowWLRLeaderboard _ = Nothing