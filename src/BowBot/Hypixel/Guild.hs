module BowBot.Hypixel.Guild where

import BowBot.BotData.Info
import BowBot.Minecraft.Basic
import qualified Data.Map as M
import BowBot.BotData.CachedSingle
import BowBot.BotData.Cached
import BowBot.Counter.Basic
import BowBot.Network.Basic
import BowBot.Hypixel.Basic
import BowBot.Discord.Utils


hypixelGuildIdInfo :: InfoType Text
hypixelGuildIdInfo = InfoType { infoName = "hypixel_guild_id", infoDefault = "", infoParse = Right }

newtype HypixelGuildMembers = HypixelGuildMembers { getHypixelGuildMemberMap :: M.Map UUID Text }

getHypixelGuildMembers :: (MonadHoistIOBotData m d r, HasAll '[Manager, CounterState] r, HasCachedData HypixelGuildMembers d, HasCache InfoField d) => m (CacheResponse HypixelGuildMembers)
getHypixelGuildMembers = getOrCalculateCacheSingle $ do
  cv <- tryIncreaseCounter HypixelApi 1
  case cv of
    Nothing -> do
      gid <- askInfo hypixelGuildIdInfo
      fmap (HypixelGuildMembers . M.fromList) <$> hypixelGuildMemberList gid
    _ -> return Nothing

