module BowBot.Hypixel.WatchlistCommands where

import BowBot.Command
import BowBot.Minecraft.Account
import qualified Data.HashMap.Strict as HM
import BowBot.BotData.Cached
import BowBot.Hypixel.Watchlist
import qualified Data.Text as T

listCommand :: Command
listCommand = Command CommandInfo
  { commandName = "list"
  , commandHelpEntries = [HelpEntry { helpUsage = "list", helpDescription = "list all players on the watchlist", helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 2
  } $ noArguments $ do
    watchlist <- getWatchlist
    cache <- getCacheMap
    respond $ "**Players in watchList:**\n```\n" <> T.unwords (map (head . mcNames . (cache HM.!)) watchlist) <> "```"

onlineCommand :: Command
onlineCommand = Command CommandInfo
  { commandName = "online"
  , commandHelpEntries = [HelpEntry { helpUsage = "online", helpDescription = "list all people from the watchlist currently in Bow Duels", helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 30
  } $ noArguments $ do
    res <- getOnlinePlayers
    cache <- getCacheMap
    let showOnline online = case T.unwords (map (head . mcNames . (cache HM.!)) online) of
          "" -> "None of the watchListed players are currently in bow duels."
          str -> str
    case res of
      Just online -> respond $ "**Players in watchList:**\n```\n" <> showOnline online <> "```"
      Nothing -> respond "**Processing list of online players. Please send command again later.**"