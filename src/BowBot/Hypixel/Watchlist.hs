{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module BowBot.Hypixel.Watchlist where

import BowBot.BotData.Cached
import BowBot.Minecraft.Account
import qualified Data.HashMap.Strict as HM
import BowBot.Minecraft.Basic
import BowBot.Network.Basic
import BowBot.BotData.CachedSingle
import BowBot.Counter.Basic
import BowBot.Hypixel.Basic
import Control.Concurrent.Async (mapConcurrently)
import BowBot.Utils


getWatchlist :: (MonadIOBotData m d r, HasCache MinecraftAccount d) => m [MinecraftAccount]
getWatchlist = filter mcHypixelWatchlist . HM.elems <$> getCacheMap

newtype HypixelOnlinePlayers = HypixelOnlinePlayers { getHypixelOnlinePlayersList :: [UUID] }

isInBowDuels :: (MonadIOReader m r, Has Manager r) => UUID -> m (Maybe Bool)
isInBowDuels uuid = hypixelWithPlayerStatus uuid $ \o -> do
  session <- o .: "session"
  mode :: Maybe Text <- session .:? "mode"
  return $ mode == Just "DUELS_BOW_DUEL"

getHypixelOnlinePlayers :: (MonadHoistIOBotData m d r, HasAll '[Manager, CounterState] r, HasCachedData HypixelOnlinePlayers d, HasCache MinecraftAccount d) => m (CacheResponse HypixelOnlinePlayers)
getHypixelOnlinePlayers = getOrCalculateCacheSingle $ do
  watchlist <- getWatchlist
  cv <- tryIncreaseCounter HypixelApi (fromIntegral $ length watchlist)
  ctx <- ask
  case cv of
    Nothing -> liftIO $ fmap (Just . HypixelOnlinePlayers . map snd . filter fst) $ flip mapConcurrently watchlist $ \acc -> flip runReaderT ctx $ do
      inBowDuels <- isInBowDuels (mcUUID acc)
      pure (inBowDuels == Just True, mcUUID acc)
    _ -> return Nothing
