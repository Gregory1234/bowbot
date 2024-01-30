module BowBot.BotData.Download where

import BowBot.BotData.Info
import BowBot.Counter.Basic
import BowBot.Hypixel.Basic
import Network.HTTP.Conduit (Manager)
import BowBot.Hypixel.Leaderboard
import BowBot.Hypixel.Guild
import BowBot.Discord.Account
import Discord
import Control.Monad.Reader
import BowBot.Hypixel.TimeStats
import Control.Concurrent.Async (concurrently_)
import BowBot.Utils
import BowBot.DB.Basic

updateBotData :: (MonadIOReader m r, HasAll [Manager, DiscordHandle, CounterState, SafeMysqlConn, InfoCache] r) => [StatsTimeRange] -> m ()
updateBotData times = (ask >>=) $ \ctx -> liftIO $ foldl1 concurrently_ $
  map (`runReaderT` ctx)
    [ updateDiscordAccountCache
    , do
      updateHypixelRoles
      updateHypixelBowLeaderboards
      forM_ times updateHypixelBowTimeStats
    ]

clearBotCaches :: (MonadIOReader m r, HasAll '[CounterState, SafeMysqlConn, Manager] r) => m ()
clearBotCaches = do
  clearCounter HypixelApi