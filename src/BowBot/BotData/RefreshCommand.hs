module BowBot.BotData.RefreshCommand where

import BowBot.Command
import BowBot.BotData.Download
import BowBot.DB.Basic
import BowBot.Utils (liftIO)
import BowBot.Network.Class (hManager)

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

updateDataCommand :: Command
updateDataCommand = Command CommandInfo
  { commandName = "dataupdate"
  , commandDescription = "" -- TODO
  , commandPerms = AdminLevel
  , commandTimeout = 120
  } $ hNoArguments $ do
    hRespond "Received"
    bdt <- CommandHandler $ \_ d _ -> return d
    liftIO $ withDB $ \conn -> refreshBotData conn bdt
    manager <- hManager
    liftIO $ updateBotData manager bdt
    hRespond "Done"