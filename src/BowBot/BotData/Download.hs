module BowBot.BotData.Download where

import BowBot.BotData.Basic
import BowBot.BotData.Info
import BowBot.Minecraft.Account
import BowBot.Account.Basic
import Database.MySQL.Simple (Connection)
import BowBot.BotData.Cached
import BowBot.DB.Basic (withDB)
import BowBot.Counter.Basic
import BowBot.Hypixel.Basic
import Network.HTTP.Conduit (Manager)
import BowBot.Hypixel.Leaderboard
import BowBot.Hypixel.Guild
import BowBot.Discord.Account
import Discord
import Control.Monad.Reader
import BowBot.Hypixel.TimeStats
import BowBot.Hypixel.Watchlist
import Control.Concurrent.Async (concurrently_)
import BowBot.Utils


emptyBotData :: STM BotData
emptyBotData = do
  infoFieldCache <- newCache
  minecraftAccountCache <- newCache
  bowBotAccountCache <- newCache
  discordAccountsCache <- newCache
  return BotData {..}

refreshBotData :: (MonadIOBotData m BotData r, Has Connection r) => m ()
refreshBotData = do
  refreshCache @InfoField
  refreshCache @MinecraftAccount
  refreshCache @BowBotAccount
  refreshCache @DiscordAccount

updateBotData :: (MonadIOBotData m BotData r, HasAll [Manager, DiscordHandle, CounterState, Connection] r) => [StatsTimeRange] -> m ()
updateBotData times = (ask >>=) $ \ctx -> liftIO $ foldl1 concurrently_ $
  map (`runReaderT` ctx)
    [ updateDiscordAccountCache
    , do
      updateHypixelRoles
      updateHypixelBowLeaderboards
      forM_ times updateHypixelBowTimeStats
    ]

clearBotDataCaches :: (MonadIOReader m r, HasAll '[CounterState, Connection, Manager] r) => m ()
clearBotDataCaches = do
  clearCounter HypixelApi
  clearOnlinePlayers

downloadBotData :: IO BotData
downloadBotData = do
  bdt <- atomically emptyBotData
  withDB $ \conn -> runReaderT refreshBotData (conn, bdt)
  return bdt