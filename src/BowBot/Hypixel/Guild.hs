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
import BowBot.Discord.Utils (MonadHoistIO)


hypixelGuildIdInfo :: InfoType String
hypixelGuildIdInfo = InfoType { infoName = "hypixel_guild_id", infoDefault = "", infoParse = Right }

newtype HypixelGuildMembers = HypixelGuildMembers { getHypixelGuildMemberMap :: M.Map UUID String }

getHypixelGuildMembers :: (MonadIO m, MonadReader r m, Has Manager r, MonadCacheSingle HypixelGuildMembers m, MonadCache InfoField m, MonadCounter HypixelApi m, MonadHoistIO m) => m (CacheResponse HypixelGuildMembers)
getHypixelGuildMembers = getOrCalculateCacheSingle $ do
  cv <- tryIncreaseCounter HypixelApi 1
  case cv of
    Nothing -> do
      gid <- hInfoDB hypixelGuildIdInfo
      fmap (HypixelGuildMembers . M.fromList) <$> hypixelGuildMemberList gid
    _ -> return Nothing

