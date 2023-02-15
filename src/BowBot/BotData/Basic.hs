module BowBot.BotData.Basic where

import BowBot.BotData.Info
import BowBot.BotData.Cached
import BowBot.Minecraft.Account
import BowBot.Command.Basic
import BowBot.Account.Basic
import BowBot.Hypixel.Basic
import BowBot.Settings.Basic
import BowBot.Hypixel.Leaderboard
import BowBot.Discord.Roles
import BowBot.BotData.CachedSingle
import BowBot.Hypixel.Guild
import BowBot.Discord.Account
import BowBot.Hypixel.TimeStats
import Data.Default
import BowBot.Hypixel.Watchlist
import BowBot.Birthday.Basic
import BowBot.Snipe.Basic
import BowBot.BotData.HasData

data BotData = BotData
  { infoFieldCache :: DatabaseCache InfoField
  , minecraftAccountCache :: DatabaseCache MinecraftAccount
  , bowBotAccountCache :: DatabaseCache BowBotAccount
  , hypixelGuildMembersCache :: CachedData HypixelGuildMembers
  , discordAccountsCache :: DatabaseCache DiscordAccount
  , hypixelOnlinePlayersCache :: CachedData HypixelOnlinePlayers
  , snipeCache :: DatabaseCache SnipeMessage
  }

instance Has (DatabaseCache InfoField) BotData where
  getter = infoFieldCache
  modifier f x = x { infoFieldCache = f $ infoFieldCache x }

instance Has (DatabaseCache MinecraftAccount) BotData where
  getter = minecraftAccountCache
  modifier f x = x { minecraftAccountCache = f $ minecraftAccountCache x }

instance Has (DatabaseCache BowBotAccount) BotData where
  getter = bowBotAccountCache
  modifier f x = x { bowBotAccountCache = f $ bowBotAccountCache x }

instance Has (CachedData HypixelGuildMembers) BotData where
  getter = hypixelGuildMembersCache
  modifier f x = x { hypixelGuildMembersCache = f $ hypixelGuildMembersCache x }

instance Has (DatabaseCache DiscordAccount) BotData where
  getter = discordAccountsCache
  modifier f x = x { discordAccountsCache = f $ discordAccountsCache x }

instance Has (CachedData HypixelOnlinePlayers) BotData where
  getter = hypixelOnlinePlayersCache
  modifier f x = x { hypixelOnlinePlayersCache = f $ hypixelOnlinePlayersCache x }

instance Has (DatabaseCache SnipeMessage) BotData where
  getter = snipeCache
  modifier f x = x { snipeCache = f $ snipeCache x }

instance HasBotData BotData BotData