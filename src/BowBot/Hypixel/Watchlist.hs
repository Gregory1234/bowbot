{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.Hypixel.Watchlist where

import BowBot.BotData.Cached
import BowBot.Minecraft.Account
import qualified Data.HashMap.Strict as HM
import BowBot.Minecraft.Basic
import BowBot.Network.Class
import BowBot.BotData.CachedSingle
import BowBot.BotData.Counter
import BowBot.Hypixel.Basic
import Control.Concurrent.Async (mapConcurrently)
import BowBot.Utils
import BowBot.Network.Monad (runNetworkT)
import Data.Aeson


getWatchlist :: MonadCache MinecraftAccount m => m [MinecraftAccount]
getWatchlist = filter mcHypixelWatchlist . HM.elems <$> getCacheMap

newtype HypixelOnlinePlayers = HypixelOnlinePlayers { getHypixelOnlinePlayersList :: [UUID] }

isInBowDuels :: MonadNetwork m => UUID -> m (Maybe Bool)
isInBowDuels uuid = hypixelWithPlayerStatus uuid $ \o -> do
  session <- o .: "session"
  mode :: Maybe String <- session .:? "mode"
  return $ mode == Just "DUELS_BOW_DUEL"

getHypixelOnlinePlayers :: (MonadNetwork m, MonadCacheSingle HypixelOnlinePlayers m, MonadCache MinecraftAccount m, MonadCounter HypixelApi m) => m (CacheResponse HypixelOnlinePlayers)
getHypixelOnlinePlayers = getOrCalculateCacheSingle $ do
  watchlist <- getWatchlist
  cv <- tryIncreaseCounter HypixelApi (fromIntegral $ length watchlist)
  manager <- hManager
  case cv of
    Nothing -> liftIO $ fmap (Just . HypixelOnlinePlayers . map snd . filter fst) $ flip mapConcurrently watchlist $ \acc -> flip runNetworkT manager $ do
      inBowDuels <- isInBowDuels (mcUUID acc)
      pure (inBowDuels == Just True, mcUUID acc)
    _ -> return Nothing
