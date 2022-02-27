{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module BowBot.Stats.HypixelBow(
  module BowBot.Stats.HypixelBow, module BowBot.Settings, module Data.Map
) where

import BowBot.Utils
import BowBot.Settings
import BowBot.API
import BowBot.DB
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ratio ((%))
import Data.Map (toList, Map, fromList)
import BowBot.API.Hypixel

data HypixelDivisionRankName
  = HRookie | HIron | HGold | HDiamond | HMaster | HLegend | HGrandmaster | HGodlike | HCelestial | HDivine | HAscended
  deriving (Show, Enum, Eq, Ord, Bounded)

data HypixelDivisionRankLevel = HL1 | HL2 | HL3 | HL4 | HL5 deriving (Show, Enum, Eq, Ord, Bounded) -- TODO: allegedly ascended goes to 50

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
    showName HCelestial = "Celestial"
    showName HDivine = "Divine"
    showName HAscended = "Ascended"
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
divisionRankBaseWins HCelestial = 25000
divisionRankBaseWins HDivine = 50000
divisionRankBaseWins HAscended = 100000

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
  | x < 50000 = Just $ calc HCelestial
  | x < 100000 = Just $ calc HDivine
  | otherwise = Just $ HypixelDivisionRank HAscended HL1
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

data HypixelBowLeaderboards = HypixelBowLeaderboards
  { bowLbWins :: Integer, bowLbLosses :: Integer, bowLbWinstreak :: Integer } deriving (Show)

requestHypixelBowStats :: APIMonad m => UUID -> m (Maybe HypixelBowStats)
requestHypixelBowStats uuid = hypixelWithPlayerData uuid $ \o -> do
    pl <- o .: "player"
    stats <- pl .: "stats"
    duelsStats <- stats .:? "Duels"
    bowWins <- fromMaybe 0 <$> for duelsStats (\x -> x .:? "bow_duel_wins" .!= 0)
    bowLosses <- fromMaybe 0 <$> for duelsStats (\x -> x .:? "bow_duel_losses" .!= 0)
    currentWinstreak <- fromMaybe 0 <$> for duelsStats (\x -> x .:? "current_bow_winstreak" .!= 0)
    bestWinstreak <- fromMaybe 0 <$> for duelsStats (\x -> x .:? "best_winstreak_mode_bow_duel" .!= 0)
    bestDailyWinstreak <- fromMaybe 0 <$> for duelsStats (\x -> x .:? "duels_winstreak_best_bow_duel" .!= 0)
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

hypixelBowStatsToLeaderboards :: HypixelBowStats -> HypixelBowLeaderboards
hypixelBowStatsToLeaderboards HypixelBowStats {..} = HypixelBowLeaderboards
  { bowLbWins = bowWins, bowLbLosses = bowLosses, bowLbWinstreak = bestWinstreak }

getHypixelBowLeaderboard :: DBMonad m => m (Map UUID HypixelBowLeaderboards)
getHypixelBowLeaderboard = do
  res :: [(String, Integer, Integer, Integer)] <- hQueryLog "SELECT `minecraft`, `bowWins`, `bowLosses`, `bowWinstreak` FROM `statsDEV`" ()
  return $ fromList $ flip fmap res $ \case
    (UUID -> uuid, bowLbWins, bowLbLosses, bowLbWinstreak) -> (uuid, HypixelBowLeaderboards {..})

data TimeStatsType = DailyStats | WeeklyStats | MonthlyStats deriving (Show, Eq)

timeStatsTypeName :: TimeStatsType -> String
timeStatsTypeName DailyStats = "Day"
timeStatsTypeName WeeklyStats = "Week"
timeStatsTypeName MonthlyStats = "Month"

updateHypixelBowLeaderboard :: (DBMonad m, MonadHoistIO m) => [TimeStatsType] -> Map UUID HypixelBowLeaderboards -> m ()
updateHypixelBowLeaderboard extraModes lb = hTransaction $ do
  let wslb = mapMaybe helperWS $ toList lb
  let nowslb = mapMaybe helperNoWS $ toList lb
  let extralb = helperExtra <$> toList lb
  _ <- hExecuteManyLog "INSERT INTO `statsDEV` (`minecraft`, `bowWins`, `bowLosses`, `bowWinstreak`) VALUES (?,?,?,?) ON DUPLICATE KEY UPDATE `bowWins`=VALUES(`bowWins`), `bowLosses`=VALUES(`bowLosses`), `bowWinstreak`=VALUES(`bowWinstreak`)" wslb
  _ <- hExecuteManyLog "INSERT INTO `statsDEV` (`minecraft`, `bowWins`, `bowLosses`) VALUES (?,?,?) ON DUPLICATE KEY UPDATE `bowWins`=VALUES(`bowWins`), `bowLosses`=VALUES(`bowLosses`)" nowslb
  for_ extraModes $ \t -> hExecuteManyLog (replaceQuery "TIME" (timeStatsTypeName t) "INSERT INTO `statsDEV` (`minecraft`, `lastTIMEWins`, `lastTIMELosses`) VALUES (?,?,?) ON DUPLICATE KEY UPDATE `lastTIMEWins`=VALUES(`lastTIMEWins`), `lastTIMELosses`=VALUES(`lastTIMELosses`)") extralb
    where
      helperNoWS (UUID uuid, HypixelBowLeaderboards {..})
        | bowLbWinstreak == 0 = Just (uuid, bowLbWins, bowLbLosses)
        | otherwise = Nothing
      helperWS (UUID uuid, HypixelBowLeaderboards {..})
        | bowLbWinstreak /= 0 = Just (uuid, bowLbWins, bowLbLosses, bowLbWinstreak)
        | otherwise = Nothing
      helperExtra (UUID uuid, HypixelBowLeaderboards {..}) = (uuid, bowLbWins, bowLbLosses)

data HypixelBowTimeStats = HypixelBowTimeStats
  { bowTimeWins :: Integer,
    bowTimeLosses :: Integer
  } deriving (Show)

requestHypixelBowTimeStats :: DBMonad m => HypixelBowStats -> UUID -> TimeStatsType -> m (Maybe HypixelBowTimeStats)
requestHypixelBowTimeStats HypixelBowStats {..} (UUID uuid) t = do
  res :: [(Integer, Integer)] <- hQueryLog (replaceQuery "TIME" (timeStatsTypeName t) "SELECT `lastTIMEWins`, `lastTIMELosses` FROM `statsDEV` WHERE `minecraft` = ?") (Only uuid)
  return $ case res of
    [(-1, -1)] -> Nothing
    [(lastWins, lastLosses)] -> Just HypixelBowTimeStats {bowTimeWins = bowWins - lastWins, bowTimeLosses = bowLosses - lastLosses}
    _ -> Nothing

showMaybeHypixelBowTimeStats :: Settings -> TimeStatsType -> Maybe HypixelBowTimeStats -> String
showMaybeHypixelBowTimeStats _ DailyStats Nothing = "**Daily data isn't avaliable yet for this player! Wait until tomorrow!**"
showMaybeHypixelBowTimeStats _ WeeklyStats Nothing = "**Weekly data isn't avaliable yet for this player! Wait until next week!**"
showMaybeHypixelBowTimeStats _ MonthlyStats Nothing = "**Monthly data isn't avaliable yet for this player! Wait until next month!**"
showMaybeHypixelBowTimeStats s t (Just v) = showHypixelBowTimeStats s t v

showHypixelBowTimeStats :: Settings -> TimeStatsType -> HypixelBowTimeStats -> String
showHypixelBowTimeStats Settings {..} bowTimeType HypixelBowTimeStats {..} = unlines $ catMaybes
  [ onlyIf sWins
  $ " - *Bow Duels " ++ time ++ " Wins:* **"
  ++ show bowTimeWins
  ++ "**"
  , onlyIf sLosses
  $ " - *Bow Duels " ++ time ++ " Losses:* **"
  ++ show bowTimeLosses
  ++ "**"
  , onlyIf (sense sWLR (bowTimeWins + bowTimeLosses /= 0))
  $ " - *Bow Duels " ++ time ++ " Win/Loss Ratio:* **"
  ++ winLossRatio
  ++ "**"
  ]
  where
    time = timeStatsTypeShowName bowTimeType
    timeStatsTypeShowName DailyStats = "Daily"
    timeStatsTypeShowName WeeklyStats = "Weekly"
    timeStatsTypeShowName MonthlyStats = "Monthly"
    sense Always _ = True
    sense Never _ = False
    sense WhenSensible x = x
    onlyIf True a = Just a
    onlyIf False _ = Nothing
    winLossRatio = showWLR bowTimeWins bowTimeLosses

banHypixelBowLeaderboard :: (DBMonad m, MonadHoistIO m) => UUID -> m Bool
banHypixelBowLeaderboard (UUID uuid) = hTransaction $ do
  changed <- hExecuteLog "UPDATE `minecraftDEV` SET `hypixel`='ban' WHERE `uuid`=?" (Only uuid)
  void $ hExecuteLog "DELETE FROM `statsDEV` WHERE `minecraft`=?" (Only uuid)
  return (changed > 0)

fullUpdateHypixelBowStats :: (APIMonad m, DBMonad m, MonadHoistIO m) => UUID -> m ()
fullUpdateHypixelBowStats uuid = do
  stats <- requestHypixelBowStats uuid
  for_ stats $ \x -> updateHypixelBowLeaderboard [] $ fromList [(uuid, hypixelBowStatsToLeaderboards x)]

hypixelBowWinsLeaderboard :: HypixelBowLeaderboards -> Maybe (Integer, String)
hypixelBowWinsLeaderboard HypixelBowLeaderboards {..} | bowLbWins >= 500 = Just (bowLbWins, show bowLbWins)
hypixelBowWinsLeaderboard _ = Nothing

hypixelBowLossesLeaderboard :: HypixelBowLeaderboards -> Maybe (Integer, String)
hypixelBowLossesLeaderboard HypixelBowLeaderboards {..} | bowLbWins >= 500 = Just (bowLbLosses, show bowLbLosses)
hypixelBowLossesLeaderboard _ = Nothing

hypixelBowWinstreakLeaderboard :: HypixelBowLeaderboards -> Maybe (Integer, String)
hypixelBowWinstreakLeaderboard HypixelBowLeaderboards {..} | bowLbWinstreak >= 50 = Just (bowLbWinstreak, show bowLbWinstreak)
hypixelBowWinstreakLeaderboard _ = Nothing

hypixelBowWLRLeaderboard :: HypixelBowLeaderboards -> Maybe (Integer, String)
hypixelBowWLRLeaderboard HypixelBowLeaderboards {..} | bowLbWins >= bowLbLosses, bowLbWins >= 150 = Just (if bowLbLosses == 0 then bowLbWins*100000000 else (bowLbWins*10000) `div` bowLbLosses, showWLR bowLbWins bowLbLosses)
hypixelBowWLRLeaderboard _ = Nothing