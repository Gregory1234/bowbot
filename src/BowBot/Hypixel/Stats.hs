module BowBot.Hypixel.Stats where

import BowBot.Settings.Basic
import BowBot.Network.Basic
import BowBot.Minecraft.Basic (UUID)
import BowBot.Hypixel.Basic
import BowBot.Utils
import BowBot.Hypixel.Division
import Data.Ratio ((%))
import Data.Time.Clock.POSIX (getCurrentTime)
import Data.Time.Clock (UTCTime)
import BowBot.Discord.Utils (discordFormatTimestampFull)
import qualified Data.Text as T

data CachedMaybe a = NewJust a | CachedJust (Maybe UTCTime) a | CachedNothing deriving (Show, Eq)

isCachedNothing :: CachedMaybe a -> Bool
isCachedNothing CachedNothing = True
isCachedNothing _ = False

isAnyJust :: CachedMaybe a -> Bool
isAnyJust = not . isCachedNothing

cachedMaybe :: a -> (b -> a) -> (Maybe UTCTime -> b -> a) -> CachedMaybe b -> a
cachedMaybe n _ _ CachedNothing = n
cachedMaybe _ f _ (NewJust a) = f a
cachedMaybe _ _ f (CachedJust t a) = f t a

completeCachedMaybe :: Maybe UTCTime -> CachedMaybe a -> Maybe a -> CachedMaybe a
completeCachedMaybe time CachedNothing (Just a) = CachedJust time a
completeCachedMaybe _ c _ = c

cachedTimestamp :: UTCTime -> CachedMaybe a -> Maybe UTCTime
cachedTimestamp time (NewJust _) = Just time
cachedTimestamp _ (CachedJust time _) = time
cachedTimestamp _ CachedNothing = Nothing

cachedToMaybe :: CachedMaybe a -> Maybe a
cachedToMaybe = cachedMaybe Nothing Just (const Just)

data HypixelBowStats = HypixelBowStats
  { bowWins :: !Integer,
    bowLosses :: !Integer,
    bestWinstreak :: !(CachedMaybe Integer),
    currentWinstreak :: !(Maybe Integer),
    bestDailyWinstreak :: !(Maybe Integer),
    bowHits :: !Integer,
    bowShots :: !Integer,
    bowStatsTimestamp :: UTCTime
  } deriving (Show)


requestHypixelBowStats :: (MonadIOReader m r, Has Manager r) => UUID -> m (Maybe HypixelBowStats)
requestHypixelBowStats uuid = do
  bowStatsTimestamp <- liftIO getCurrentTime
  hypixelWithPlayerData uuid $ \o -> do
    pl <- o .: "player"
    stats <- pl .: "stats"
    duelsStats <- stats .:? "Duels"
    bowWins <- fromMaybe 0 <$> for duelsStats (\x -> x .:? "bow_duel_wins" .!= 0)
    bowLosses <- fromMaybe 0 <$> for duelsStats (\x -> x .:? "bow_duel_losses" .!= 0)
    bestWinstreak <- (\a -> if a == Just 0 && bowWins > 0 then CachedNothing else maybe CachedNothing NewJust a) <$> for duelsStats (\x -> x .:? "best_winstreak_mode_bow_duel" .!= 0)
    currentWinstreak <- (\a -> if isCachedNothing bestWinstreak then Nothing else a) <$> for duelsStats (\x -> x .:? "current_bow_winstreak" .!= 0)
    bestDailyWinstreak <- (\a -> if isCachedNothing bestWinstreak then Nothing else a) <$> for duelsStats (\x -> x .:? "duels_winstreak_best_bow_duel" .!= 0)
    bowHits <- fromMaybe 0 <$> for duelsStats (\x -> x .:? "bow_duel_bow_hits" .!= 0)
    bowShots <- fromMaybe 0 <$> for duelsStats (\x -> x .:? "bow_duel_bow_shots" .!= 0)
    return HypixelBowStats {..}


showHypixelBowStats :: Settings -> HypixelBowStats -> Text
showHypixelBowStats Settings {..} HypixelBowStats {..} = T.unlines $ catMaybes
  [ onlyIfBin sWins
  $ "- *Bow Duels Wins:* **"
  <> showt bowWins
  <> "**" <> maybe "" (\x -> " (**Bow " <> divisionRankName x <> "**)") (divisionRankFromWins bowWins)
  , onlyIfBin sLosses
  $ "- *Bow Duels Losses:* **"
  <> showt bowLosses
  <> "**"
  , onlyIfTer sWLR (bowWins + bowLosses /= 0)
  $ "- *Bow Duels Win/Loss Ratio:* **"
  <> winLossRatio
  <> "**"
  , onlyIfTer sWinsUntil (bowLosses /= 0)
  $ "- *Bow Duels Wins until "
  <> nextWinLossRatio
  <> " WLR:* **"
  <> winsRemaining
  <> "**"
  , onlyIfTer sBestStreak (isAnyJust bestWinstreak)
  $ "- *Best Bow Duels Winstreak:* **"
  <> cachedMaybe "API DISABLED" showt (\t -> (<>" (CACHED" <> maybe "" ((\s -> " **" <> s <> "**") . discordFormatTimestampFull) t <> ")") . pack . show) bestWinstreak
  <> "**"
  , onlyIfTer sCurrentStreak (isJust currentWinstreak)
  $ "- *Current Bow Duels Winstreak:* **"
  <> maybe "API DISABLED" showt currentWinstreak
  <> "**"
  , onlyIfTer sBestDailyStreak (isJust bestDailyWinstreak)
  $ "- *Best Daily Bow Duels Winstreak(?):* **"
  <> maybe "API DISABLED" showt bestDailyWinstreak
  <> "**"
  , onlyIfBin sBowHits
  $ "- *Bow Hits in Bow Duels:* **"
  <> showt bowHits
  <> "**"
  , onlyIfBin sBowShots
  $ "- *Bow Shots in Bow Duels:* **"
  <> showt bowShots
  <> "**"
  , onlyIfTer sAccuracy (bowShots /= 0)
  $ "- *Bow Accuracy:* **"
  <> accuracy
  <> "**"
  ]
  where
    winLossRatio = showWLR (WLR bowWins bowLosses)
    nextWinLossRatio
      | bowLosses == 0 = "∞"
      | otherwise = showt $ (bowWins `div` bowLosses) + 1
    winsRemaining
      | bowWins == 0, bowLosses == 0 = "1"
      | bowLosses == 0 = "N/A"
      | otherwise = showt (bowLosses - (bowWins `mod` bowLosses))
    accuracy
      | bowShots == 0 = "N/A"
      | otherwise = showt (round ((bowHits*100) % bowShots) :: Integer) <> "%"