module BowBot.BotData.Basic where

import BowBot.BotData.Info
import BowBot.BotData.Cached
import BowBot.Minecraft.Account
import BowBot.Account.Basic
import BowBot.Discord.Account

data BotData = BotData
  { infoFieldCache :: DatabaseCache InfoField
  , minecraftAccountCache :: DatabaseCache MinecraftAccount
  , bowBotAccountCache :: DatabaseCache BowBotAccount
  , discordAccountsCache :: DatabaseCache DiscordAccount
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

instance Has (DatabaseCache DiscordAccount) BotData where
  getter = discordAccountsCache
  modifier f x = x { discordAccountsCache = f $ discordAccountsCache x }

instance HasBotData BotData BotData