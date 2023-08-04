{-# LANGUAGE QuasiQuotes #-}
module BowBot.Hypixel.WatchlistCommands where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Hypixel.Watchlist
import qualified Data.Text as T
import BowBot.DB.Typed
import BowBot.Discord.Utils
import BowBot.Minecraft.Basic

listCommand :: Command
listCommand = Command CommandInfo
  { commandName = "list"
  , commandHelpEntries = [HelpEntry { helpUsage = "list", helpDescription = "list all players on the watchlist", helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 2
  } $ noArguments $ do
    watchlist <- getWatchlistAccounts
    respond $ "**Players in watchList:**\n```\n" <> T.unwords (map (head . mcNames) watchlist) <> "```"

onlineCommand :: Command
onlineCommand = Command CommandInfo
  { commandName = "online"
  , commandHelpEntries = [HelpEntry { helpUsage = "online", helpDescription = "list all people from the watchlist currently in Bow Duels", helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 30
  } $ noArguments $ do
    online <- liftMaybe "**Processing list of online players. Please send command again later.**" =<< getOnlinePlayers
    mcs <- queryLogT [mysql|SELECT MinecraftAccount FROM `minecraft` WHERE `uuid` IN online|]
    let onlineStr = case T.unwords (map (head . mcNames) mcs) of
          "" -> "None of the watchListed players are currently in bow duels."
          str -> str
    respond $ "**Players in watchList:**\n```\n" <> onlineStr <> "```"