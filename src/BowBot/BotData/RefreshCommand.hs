module BowBot.BotData.RefreshCommand where

import BowBot.Command
import BowBot.BotData.Download
import BowBot.DB.Basic
import BowBot.Hypixel.TimeStats
import BowBot.Hypixel.Announce
import BowBot.Discord.Utils
import BowBot.BotData.Cached (getCacheMap)
import BowBot.Minecraft.Account
import Control.Monad.Error.Class (liftEither)
import Text.Read (readEither)

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
  oldDaily <- getCacheMap
  updateBotData times
  when (DailyStats `elem` times) $ announceMilestones oldDaily

updateNamesCommand :: Command
updateNamesCommand = Command CommandInfo
  { commandName = "namesupdate"
  , commandHelpEntries = [HelpEntry { helpUsage = "namesupdate [hour]", helpDescription = "update Bow Bot Minecraft username data", helpGroup = "normal" }]
  , commandPerms = AdminLevel
  , commandTimeout = 3600
  } $ oneArgument (liftEither . readEither) $ \hour -> do
    respond "Received"
    updateMinecraftAccountCache hour
    respond "Done"