module BowBot.BotData.RefreshCommand where

import BowBot.Command
import BowBot.BotData.Download
import BowBot.DB.Basic
import BowBot.Utils (liftIO)

refreshDataCommand :: Command () ()
refreshDataCommand = Command () CommandInfo
  { commandName = "datarefresh"
  , commandDescription = "" -- TODO
  , commandPerms = AdminLevel
  , commandTimeout = 120
  } $ withArgs $ \() -> do
    bdt <- CommandHandler $ \_ d _ -> return d
    liftIO $ withDB $ \conn -> updateBotData conn bdt
    hRespond "Done"
