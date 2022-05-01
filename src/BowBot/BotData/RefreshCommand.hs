module BowBot.BotData.RefreshCommand where

import BowBot.Command
import BowBot.BotData.Download
import BowBot.DB.Basic
import BowBot.Utils (liftIO)
import BowBot.Network.Class (hManager)
import BowBot.Discord.Class (liftDiscord)
import BowBot.Hypixel.TimeStats

refreshDataCommand :: Command
refreshDataCommand = Command CommandInfo
  { commandName = "datarefresh"
  , commandDescription = "" -- TODO
  , commandPerms = AdminLevel
  , commandTimeout = 120
  } $ hNoArguments $ do
    hRespond "Received"
    bdt <- CommandHandler $ \_ d _ -> return d
    liftIO $ withDB $ \conn -> refreshBotData conn bdt
    hRespond "Done"

updateDataCommand :: [StatsTimeRange] -> String -> Command
updateDataCommand times name = Command CommandInfo
  { commandName = name
  , commandDescription = "" -- TODO
  , commandPerms = AdminLevel
  , commandTimeout = 120
  } $ hNoArguments $ do
    hRespond "Received"
    bdt <- CommandHandler $ \_ d _ -> return d
    liftIO $ withDB $ \conn -> refreshBotData conn bdt
    manager <- hManager
    liftDiscord $ updateBotData times manager bdt
    hRespond "Done"