{-# LANGUAGE TypeApplications #-}

module BowBot.Hypixel.WatchlistCommands where

import BowBot.Command
import BowBot.Minecraft.Account
import qualified Data.HashMap.Strict as HM
import Data.Proxy
import BowBot.BotData.Cached
import BowBot.Hypixel.Watchlist
import BowBot.BotData.CachedSingle

listCommand :: Command
listCommand = Command CommandInfo
  { commandName = "list"
  , commandUsage = "list"
  , commandDescription = "list all players on the watchlist"
  , commandPerms = DefaultLevel
  , commandTimeout = 2
  , commandGroup = "normal"
  } $ hNoArguments $ do
    watchlist <- getWatchlist
    hRespond $ "**Players in watchList:**\n```\n" ++ unwords (map (head . mcNames) watchlist) ++ "```"

onlineCommand :: Command
onlineCommand = Command CommandInfo
  { commandName = "online"
  , commandUsage = "online"
  , commandDescription = "list all people from the watchlist currently in Bow Duels"
  , commandPerms = DefaultLevel
  , commandTimeout = 30
  , commandGroup = "normal"
  } $ hNoArguments $ do
    res <- getHypixelOnlinePlayers
    cache <- getCacheMap (Proxy @MinecraftAccount)
    let showOnline online = case unwords (map (head . mcNames . (cache HM.!)) (getHypixelOnlinePlayersList online)) of
          [] -> "None of the watchListed players are currently in bow duels."
          str -> str
    case res of
      CacheFresh online -> hRespond $ "**Players in watchList:**\n```\n" ++ showOnline online ++ "```"
      CacheOld online -> hRespond $ "**Players in watchList:** (cached response)\n```\n" ++ showOnline online ++ "```"
      CacheBusy -> hRespond "**Processing list of online players. Please send command again later.**"
      CacheFailed -> hRespond "**Something went wrong!**"