{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.Birthday where

import BowBot.Utils
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import BowBot.API
import Discord.Types

data BirthdayDate = BirthdayDate { birthdayDay :: Int, birthdayMonth :: Int }

birthdayString :: BirthdayDate -> String
birthdayString BirthdayDate {..} = pad' False '0' 2 (show birthdayDay) ++ "." ++ pad' False '0' 2 (show birthdayMonth)

birthdayFromString :: String -> Maybe BirthdayDate
birthdayFromString str = case splitOn "." str of
  [readMaybe -> Just a, readMaybe -> Just b] -> Just $ BirthdayDate a b
  _ -> Nothing

currentBirthdayDate :: IO BirthdayDate
currentBirthdayDate = fromJust . birthdayFromString <$> getTime "%d.%m"

getBirthdayPeople :: Manager -> BirthdayDate -> IO (Maybe [UserId])
getBirthdayPeople manager bd = do
  res <- sendDB manager "discord/birthday/get.php" ["date=" ++ birthdayString bd]
  decodeParse res $ \o -> do
      dt <- o .: "data"
      for dt $ \s -> do
        (readMaybe -> Just x) <- pure s
        return x