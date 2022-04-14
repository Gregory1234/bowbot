{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module BowBot.BotData.Download where

import BowBot.BotData.Basic
import BowBot.BotData.Info
import BowBot.Minecraft.Account
import Control.Concurrent.STM (STM, newTVar, atomically)
import Data.HashMap.Strict (empty)
import Database.MySQL.Simple (Connection)
import BowBot.BotData.Cached (refreshCache)
import Data.Proxy
import BowBot.DB.Basic (withDB)



emptyBotData :: STM BotData
emptyBotData = do
  infoFieldCache <- newTVar empty
  minecraftAccountCache <- newTVar empty
  return BotData {..}

updateBotData :: Connection -> BotData -> IO ()
updateBotData conn bdt = flip runBotDataT bdt $ do
  refreshCache conn (Proxy @InfoField)
  refreshCache conn (Proxy @MinecraftAccount)

downloadBotData :: IO BotData
downloadBotData = do
  bdt <- atomically emptyBotData
  withDB $ \conn -> updateBotData conn bdt
  return bdt