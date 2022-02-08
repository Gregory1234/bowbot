{-# LANGUAGE ViewPatterns #-}

module BowBot.Command.RankedTurfWars where

import BowBot.Command
import BowBot.RankedTurfWars
import BowBot.Minecraft
import Data.List.Split (splitOn)
import Data.List (find)
import BowBot.Settings
import Control.Monad ((<=<))
import Data.Char (toLower)

rtwStatsCommand :: Command
rtwStatsCommand = Command "rtws" DefaultLevel 20 $ do
  name <- hArg 1
  caller <- hCaller
  let search = case name of -- TODO: add autocorrect
        Nothing -> parseRTWStats <=< find ((==show (userId caller)) . (!!1) . splitOn ",")
        Just (fromPingDiscordUser -> Just did) -> parseRTWStats <=< find ((==show did) . (!!1) . splitOn ",")
        Just n -> parseRTWStats <=< find ((==map toLower n) . map toLower . (!!2) . splitOn ",")
  res <- hDiscordCache rtwData $ hData >>= hDiscord . sendRTWDataRequest
  case res of
    CacheFailed -> hRespond somethingWrongMessage
    CacheBusy -> hRespond "**Processing the RTW data. Please send command again later.**"
    CacheFresh a -> do
      hRespond $ maybe "Player stats not found!" (showRTWStats False allSettings) $ search $ lines a -- TODO: add settings customization
    CacheOld a -> do
      hRespond $ maybe "Player stats not found!" (showRTWStats False allSettings)  $ search $ lines a