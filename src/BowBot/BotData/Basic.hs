module BowBot.BotData.Basic where

import BowBot.BotData.Cached
import BowBot.Minecraft.Account
import BowBot.Discord.Account

data BotData = BotData
  { minecraftAccountCache :: DatabaseCache MinecraftAccount
  , discordAccountsCache :: DatabaseCache DiscordAccount
  }

instance Has (DatabaseCache MinecraftAccount) BotData where
  getter = minecraftAccountCache
  modifier f x = x { minecraftAccountCache = f $ minecraftAccountCache x }

instance Has (DatabaseCache DiscordAccount) BotData where
  getter = discordAccountsCache
  modifier f x = x { discordAccountsCache = f $ discordAccountsCache x }

instance HasBotData BotData BotData