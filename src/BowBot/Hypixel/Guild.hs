{-# LANGUAGE FlexibleContexts #-}

module BowBot.Hypixel.Guild where

import BowBot.BotData.Info
import BowBot.Minecraft.Basic
import qualified Data.Map as M
import BowBot.BotData.CachedSingle
import BowBot.BotData.Cached
import BowBot.BotData.Counter
import BowBot.Network.Basic
import BowBot.Hypixel.Basic


hypixelGuildIdInfo :: InfoType String
hypixelGuildIdInfo = InfoType { infoName = "hypixel_guild_id", infoDefault = "", infoParse = Right }

newtype HypixelGuildMembers = HypixelGuildMembers { getHypixelGuildMemberMap :: M.Map UUID String }

getHypixelGuildMembers :: (MonadHoistIOBotData m d r, Has Manager r, HasCachedData HypixelGuildMembers d, HasCache InfoField d, HasCounter' HypixelApi d) => m (CacheResponse HypixelGuildMembers)
getHypixelGuildMembers = getOrCalculateCacheSingle $ do
  cv <- tryIncreaseCounter HypixelApi 1
  case cv of
    Nothing -> do
      gid <- askInfo hypixelGuildIdInfo
      fmap (HypixelGuildMembers . M.fromList) <$> hypixelGuildMemberList gid
    _ -> return Nothing

