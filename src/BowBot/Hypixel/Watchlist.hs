{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.Hypixel.Watchlist where

import BowBot.BotData.Cached
import BowBot.Minecraft.Account
import qualified Data.HashMap.Strict as HM
import BowBot.Minecraft.Basic
import BowBot.Network.Basic
import BowBot.BotData.CachedSingle
import BowBot.BotData.Counter
import BowBot.Hypixel.Basic
import Control.Concurrent.Async (mapConcurrently)
import BowBot.Utils


getWatchlist :: (MonadIO m, MonadReader r m, HasBotData d r, HasCache MinecraftAccount d) => m [MinecraftAccount]
getWatchlist = filter mcHypixelWatchlist . HM.elems <$> getCacheMap

newtype HypixelOnlinePlayers = HypixelOnlinePlayers { getHypixelOnlinePlayersList :: [UUID] }

isInBowDuels :: (MonadIO m, MonadReader r m, Has Manager r) => UUID -> m (Maybe Bool)
isInBowDuels uuid = hypixelWithPlayerStatus uuid $ \o -> do
  session <- o .: "session"
  mode :: Maybe String <- session .:? "mode"
  return $ mode == Just "DUELS_BOW_DUEL"

getHypixelOnlinePlayers :: (MonadHoistIO m, MonadReader r m, HasBotData d r, Has Manager r, HasCachedData HypixelOnlinePlayers d, HasCache MinecraftAccount d, HasCounter' HypixelApi d) => m (CacheResponse HypixelOnlinePlayers)
getHypixelOnlinePlayers = getOrCalculateCacheSingle $ do
  watchlist <- getWatchlist
  cv <- tryIncreaseCounter HypixelApi (fromIntegral $ length watchlist)
  ctx <- ask
  case cv of
    Nothing -> liftIO $ fmap (Just . HypixelOnlinePlayers . map snd . filter fst) $ flip mapConcurrently watchlist $ \acc -> flip runReaderT ctx $ do
      inBowDuels <- isInBowDuels (mcUUID acc)
      pure (inBowDuels == Just True, mcUUID acc)
    _ -> return Nothing
