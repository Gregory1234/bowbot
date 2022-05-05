module BowBot.BotData.RefreshCommand where

import BowBot.Command
import BowBot.BotData.Download
import BowBot.DB.Basic
import BowBot.Utils (liftIO)
import BowBot.Network.Class (hManager)
import BowBot.Discord.Class (liftDiscord)
import BowBot.Hypixel.TimeStats
import BowBot.BotData.Basic (BotData)
import Data.List (intercalate)
import Data.Char (toLower)

adminCommand :: Int -> String -> String -> (BotData -> CommandHandler ()) -> Command
adminCommand timeout name desc body = Command CommandInfo
  { commandName = name
  , commandHelpEntries = [HelpEntry { helpUsage = name, helpDescription = desc, helpGroup = "normal" }]
  , commandPerms = AdminLevel
  , commandTimeout = timeout
  } $ hNoArguments $ do
    hRespond "Received"
    bdt <- CommandHandler $ \_ d _ -> return d
    body bdt
    hRespond "Done"

quietAdminCommand :: Int -> String -> String -> (BotData -> CommandHandler ()) -> Command
quietAdminCommand timeout name desc body = Command CommandInfo
  { commandName = name
  , commandHelpEntries = [HelpEntry { helpUsage = name, helpDescription = desc, helpGroup = "normal" }]
  , commandPerms = AdminLevel
  , commandTimeout = timeout
  } $ hNoArguments $ do
    bdt <- CommandHandler $ \_ d _ -> return d
    body bdt

updateDataCommand :: [StatsTimeRange] -> String -> Command
updateDataCommand times name = adminCommand 3600 name ("update Bow Bot data" ++ if null times then "" else " as if it was the beginning of: " ++ intercalate ", " (map (map toLower . statsTimeRangeName) times)) $ \bdt -> do
    liftIO $ withDB $ \conn -> refreshBotData conn bdt
    manager <- hManager
    liftDiscord $ updateBotData times manager bdt