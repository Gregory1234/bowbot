module BowBot.BotData.RefreshCommand where

import BowBot.Command
import BowBot.BotData.Download
import BowBot.DB.Basic
import BowBot.Network.Basic
import BowBot.Discord.Basic
import BowBot.Hypixel.TimeStats
import Data.List (intercalate)
import Data.Char (toLower)

adminCommand :: Int -> String -> String -> CommandHandler () -> Command
adminCommand timeout name desc body = Command CommandInfo
  { commandName = name
  , commandHelpEntries = [HelpEntry { helpUsage = name, helpDescription = desc, helpGroup = "normal" }]
  , commandPerms = AdminLevel
  , commandTimeout = timeout
  } $ noArguments $ do
    respond "Received"
    body
    respond "Done"

quietAdminCommand :: Int -> String -> String -> CommandHandler () -> Command
quietAdminCommand timeout name desc body = Command CommandInfo
  { commandName = name
  , commandHelpEntries = [HelpEntry { helpUsage = name, helpDescription = desc, helpGroup = "normal" }]
  , commandPerms = AdminLevel
  , commandTimeout = timeout
  } $ noArguments body

updateDataCommand :: [StatsTimeRange] -> String -> Command
updateDataCommand times name = adminCommand 3600 name ("update Bow Bot data" ++ if null times then "" else " as if it was the beginning of: " ++ intercalate ", " (map (map toLower . statsTimeRangeName) times)) $ do
  withDB refreshBotData
  updateBotData times