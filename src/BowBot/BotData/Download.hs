{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module BowBot.BotData.Download where

import BowBot.BotData.Basic
import BowBot.BotData.Info
import BowBot.Minecraft.Account
import BowBot.Account.Basic
import Control.Concurrent.STM (STM, newTVar, atomically)
import Data.HashMap.Strict (empty)
import Database.MySQL.Simple (Connection)
import BowBot.BotData.Cached (refreshCache)
import Data.Proxy
import BowBot.DB.Basic (withDB)
import BowBot.Command.Basic (PermissionLevel)
import BowBot.BotData.Counter
import BowBot.Hypixel.Basic



emptyBotData :: STM BotData
emptyBotData = do
  infoFieldCache <- newTVar empty
  minecraftAccountCache <- newTVar empty
  permissionCache <- newTVar empty
  bowBotAccountCache <- newTVar empty
  hypixelApiCounter <- newCounter
  return BotData {..}

updateBotData :: Connection -> BotData -> IO ()
updateBotData conn bdt = flip runBotDataT bdt $ do
  refreshCache conn (Proxy @InfoField)
  refreshCache conn (Proxy @MinecraftAccount)
  refreshCache conn (Proxy @PermissionLevel)
  refreshCache conn (Proxy @BowBotAccount)

clearBotDataCaches :: BotData -> IO ()
clearBotDataCaches bdt = flip runBotDataT bdt $ do
  clearCounter (Proxy @HypixelApi)

downloadBotData :: IO BotData
downloadBotData = do
  bdt <- atomically emptyBotData
  withDB $ \conn -> updateBotData conn bdt
  return bdt