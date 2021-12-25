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
import Data.Maybe (catMaybes)
import Data.Ratio ((%))
import Data.Aeson.Types (object, (.=))
import Data.Map (toList)
import BowBot.API.Hypixel

data HypixelDivisionRankName
  = HRookie | HIron | HGold | HDiamond | HMaster | HLegend | HGrandmaster | HGodlike | HWorldElite | HWorldMaster | HWorldsBest
  deriving (Show, Enum, Eq, Ord, Bounded)

data HypixelDivisionRankLevel = HL1 | HL2 | HL3 | HL4 | HL5 deriving (Show, Enum, Eq, Ord, Bounded)

data HypixelDivisionRank = HypixelDivisionRank HypixelDivisionRankName HypixelDivisionRankLevel deriving (Show)

divisionRankName :: HypixelDivisionRank -> String
divisionRankName (HypixelDivisionRank n l) = showName n ++ showLevel l
  where
    showName HRookie = "Rookie"
    showName HIron = "Iron"
    showName HGold = "Gold"
    showName HDiamond = "Diamond"
    showName HMaster = "Master"
    showName HLegend = "Legend"
    showName HGrandmaster = "Grandmaster"
    showName HGodlike = "Godlike"
    showName HWorldElite = "World Elite"
    showName HWorldMaster = "World Master"
    showName HWorldsBest = "World's Best"
    showLevel HL1 = ""
    showLevel HL2 = " II"
    showLevel HL3 = " III"
    showLevel HL4 = " IV"
    showLevel HL5 = " V"

divisionRankBaseWins :: HypixelDivisionRankName -> Integer
divisionRankBaseWins HRookie = 50
divisionRankBaseWins HIron = 100
divisionRankBaseWins HGold = 250
divisionRankBaseWins HDiamond = 500
divisionRankBaseWins HMaster = 1000
divisionRankBaseWins HLegend = 2000
divisionRankBaseWins HGrandmaster = 5000
divisionRankBaseWins HGodlike = 10000
divisionRankBaseWins HWorldElite = 25000
divisionRankBaseWins HWorldMaster = 50000
divisionRankBaseWins HWorldsBest = 100000

divisionRankMinimumWins :: HypixelDivisionRank -> Integer
divisionRankMinimumWins (HypixelDivisionRank n l) = divisionRankBaseWins n + ((divisionRankBaseWins (succ n) - divisionRankBaseWins n) `div` 5) * fromIntegral (fromEnum l)

divisionRankFromWins :: Integer -> Maybe HypixelDivisionRank
divisionRankFromWins x
  | x < 50 = Nothing
  | x < 100 = Just $ calc HRookie
  | x < 250 = Just $ calc HIron
  | x < 500 = Just $ calc HGold
  | x < 1000 = Just $ calc HDiamond
  | x < 2000 = Just $ calc HMaster
  | x < 5000 = Just $ calc HLegend
  | x < 10000 = Just $ calc HGrandmaster
  | x < 25000 = Just $ calc HGodlike
  | x < 50000 = Just $ calc HWorldElite
  | x < 100000 = Just $ calc HWorldMaster
  | otherwise = Just $ HypixelDivisionRank HWorldsBest HL1
    where
      calc n = HypixelDivisionRank n $ toEnum $ fromIntegral $ (x - divisionRankBaseWins n) `div` ((divisionRankBaseWins (succ n) - divisionRankBaseWins n) `div` 5)

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
  data Leaderboards HypixelBowStats = HypixelBowLeaderboards
    { bowLbWins :: Integer, bowLbLosses :: Integer, bowLbWinstreak :: Integer } deriving (Show)
  requestStats Proxy uuid = hypixelWithPlayerData uuid $ \o -> do
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

  showStats Settings {..} HypixelBowStats {..} = unlines $ catMaybes
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
  getLeaderboard Proxy = do
    res <- hSendDB "stats/hypixel/leaderboard.php" []
    decodeParse res $ \o -> do
      dt <- o .: "data"
      fmap fromList $ for dt $ \s -> do
        uuid <- s .: "uuid"
        (readMaybe -> Just bowLbWins) <- s .: "bowWins"
        (readMaybe -> Just bowLbLosses) <- s .: "bowLosses"
        (readMaybe -> Just bowLbWinstreak) <- s .: "bowWinstreak"
        return (uuid, HypixelBowLeaderboards {..})
  updateLeaderboard lb = hPostDB "stats/hypixel/update.php" (object $ helper <$> toList lb)
    where
      helper (uuid, HypixelBowLeaderboards {..}) = pack uuid .= object ["bowWins" .= bowLbWins, "bowLosses" .= bowLbLosses, "bowWinstreak" .= bowLbWinstreak]
      
  banLeaderboard _ uuid = do
    res <- hSendDB "stats/hypixel/ban.php" ["uuid="++uuid]
    decodeParse res $ \o -> o .: "success"

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
hypixelBowWLRLeaderboard HypixelBowLeaderboards {..} | bowLbWins >= bowLbLosses, bowLbWins >= 150 = Just (if bowLbLosses == 0 then bowLbWins*100000000 else (bowLbWins*10000) `div` bowLbLosses, showWLR bowLbWins bowLbLosses)
hypixelBowWLRLeaderboard _ = Nothing