{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module BowBot.RankedTurfWars where

import BowBot.BotData
import Discord
import qualified Discord.Requests as R
import qualified Data.ByteString.Lazy.Char8 as BS
import Discord.Types
import BowBot.Utils
import BowBot.Command
import Data.Maybe (catMaybes)
import Data.List.Split (splitOn)

detectRTWData :: Manager -> BotData -> Message -> IO ()
detectRTWData man bdt m = do
  channel <- readProp discordBotStreamChannel bdt
  rtwBot <- readProp rtwBotId bdt
  when (userIsBot (messageAuthor m) && messageChannel m == channel && userId (messageAuthor m) == rtwBot) $ do
    let att = head $ messageAttachments m
    text <- BS.unpack <$> withDB (\conn -> sendRequestTo conn man (unpack $ attachmentUrl att) (unpack $ attachmentUrl att))
    stm $ supplyDiscordCache (rtwData bdt) (Just text)

sendRTWDataRequest :: BotData -> DiscordHandler ()
sendRTWDataRequest bdt = do
  command <- readProp rtwDataCommand bdt
  channel <- readProp discordBotStreamChannel bdt
  call_ $ R.CreateMessage channel $ pack command

data RTWStats = RTWStats { rtwName :: String, rtwWins :: Integer, rtwLosses :: Integer, rtwMVPs :: Integer, rtwElo :: Integer }

showRTWStats :: Bool -> Settings -> RTWStats -> String
showRTWStats old Settings {..} RTWStats {..} = unlines $ catMaybes
  [ Just $ "**" ++ discordEscape rtwName ++ (if old then " (cached response)" else "")++ ":**"
  , onlyIf sWins
  $ " - *RTW Elo:* **"
  ++ show rtwElo -- TODO: add elo to settings
  ++ "**"
  , onlyIf sWins
  $ " - *RTW Wins:* **"
  ++ show rtwWins
  ++ "**"
  , onlyIf sLosses
  $ " - *RTW Losses:* **"
  ++ show rtwLosses
  ++ "**"
  , onlyIf (sense sWLR (rtwWins + rtwLosses /= 0))
  $ " - *RTW Win/Loss Ratio:* **"
  ++ winLossRatio
  ++ "**"
  , onlyIf True -- TODO: add MVPs to settings
  $ " - *RTW MVPs:* **"
  ++ show rtwMVPs
  ++ "**"
  ]
  where
    sense Always _ = True
    sense Never _ = False
    sense WhenSensible x = x
    onlyIf True a = Just a
    onlyIf False _ = Nothing
    winLossRatio = showWLR rtwWins rtwLosses

parseRTWStats :: String -> Maybe RTWStats
parseRTWStats (splitOn "," -> [_, _, rtwName, readMaybe -> Just rtwElo, readMaybe -> Just rtwWins, readMaybe -> Just rtwLosses, readMaybe -> Just rtwMVPs, _]) = Just RTWStats {..}
parseRTWStats _ = Nothing