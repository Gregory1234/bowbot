{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BowBot.Birthday where

import BowBot.Utils
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import BowBot.DB
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

getBirthdayPeople :: DBMonad m => BirthdayDate -> m [UserId]
getBirthdayPeople bd = do
  res :: [Only Integer] <- hQueryLog "SELECT (`id`) FROM `discordDEV` WHERE `birthday` = ?" (Only (birthdayString bd))
  return $ fmap (fromInteger . fromOnly) res