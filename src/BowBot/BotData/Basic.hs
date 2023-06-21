module BowBot.BotData.Basic where

import BowBot.BotData.Cached
import BowBot.Minecraft.Account

data BotData = BotData
  { minecraftAccountCache :: DatabaseCache MinecraftAccount
  }

instance Has (DatabaseCache MinecraftAccount) BotData where
  getter = minecraftAccountCache
  modifier f x = x { minecraftAccountCache = f $ minecraftAccountCache x }

instance HasBotData BotData BotData