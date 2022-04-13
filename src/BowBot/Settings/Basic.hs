module BowBot.Settings.Basic where


data BoolSense = Never | WhenSensible | Always deriving (Show, Eq, Ord, Enum)

data Settings = Settings
  { sWins :: Bool, sLosses :: Bool, sWLR :: BoolSense, sWinsUntil :: BoolSense
  , sBestStreak :: BoolSense, sCurrentStreak :: BoolSense, sBestDailyStreak :: BoolSense
  , sBowHits :: Bool, sBowShots :: Bool, sAccuracy :: BoolSense
  } deriving (Show, Eq)

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