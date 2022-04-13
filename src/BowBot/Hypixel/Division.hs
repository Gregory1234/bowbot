module BowBot.Hypixel.Division where


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