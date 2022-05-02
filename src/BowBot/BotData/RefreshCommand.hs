module BowBot.BotData.RefreshCommand where

import BowBot.Command
import BowBot.BotData.Download
import BowBot.DB.Basic
import BowBot.Utils (liftIO)
import BowBot.Network.Class (hManager)
import BowBot.Discord.Class (liftDiscord)
import BowBot.Hypixel.TimeStats
import Database.MySQL.Simple (Connection)
import BowBot.BotData.Basic (BotData)

adminCommand :: String -> (BotData -> CommandHandler ()) -> Command
adminCommand name body = Command CommandInfo
  { commandName = name
  , commandDescription = "" -- TODO
  , commandPerms = AdminLevel
  , commandTimeout = 120
  } $ hNoArguments $ do
    hRespond "Received"
    bdt <- CommandHandler $ \_ d _ -> return d
    body bdt
    hRespond "Done"

quietAdminCommand :: String -> (BotData -> CommandHandler ()) -> Command
quietAdminCommand name body = Command CommandInfo
  { commandName = name
  , commandDescription = "" -- TODO
  , commandPerms = AdminLevel
  , commandTimeout = 120
  } $ hNoArguments $ do
    bdt <- CommandHandler $ \_ d _ -> return d
    body bdt

updateDataCommand :: [StatsTimeRange] -> String -> Command
updateDataCommand times name = adminCommand name $ \bdt -> do
    liftIO $ withDB $ \conn -> refreshBotData conn bdt
    manager <- hManager
    liftDiscord $ updateBotData times manager bdt