{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module BowBot.Account.Basic where

import BowBot.Minecraft.Basic (UUID(..))
import Discord.Internal.Rest (UserId)
import BowBot.BotData.Cached
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import BowBot.DB.Basic (queryLog, Only(..))
import BowBot.Utils

data BowBotAccount = BowBotAccount
  { accountId :: Integer
  , accountDiscords :: [UserId]
  , accountSelectedMinecraft :: UUID
  , accountMinecrafts :: [UUID]
  } deriving (Show, Eq)

instance Cached BowBotAccount where
  type CacheIndex BowBotAccount = Integer
  refreshCache = do
    cache <- getCache
    ids :: [Only Integer] <- queryLog "SELECT `id` FROM `people`" ()
    minecrafts :: [(Integer, Text, Bool)] <- queryLog "SELECT `id`, `minecraft`, `selected` FROM `peopleMinecraft`" ()
    discords :: [(Integer, Integer)] <- queryLog "SELECT `id`, `discord` FROM `peopleDiscord`" ()
    let minecraftsMap = M.map (\l -> let u = map (\(_,b,c) -> (UUID b,c)) l in (fst $ head $ filter snd u, map fst u)) $ groupByToMap (\(a,_,_) -> a) minecrafts
    let discordsMap = M.map (map (fromIntegral . snd)) $ groupByToMap fst discords
    let newValues = HM.fromList $ flip mapMaybe ids $ \(Only i) -> (i,) <$> do
          (accountSelectedMinecraft, accountMinecrafts) <- minecraftsMap M.!? i
          accountDiscords <- discordsMap M.!? i
          return BowBotAccount { accountId = i, ..}
    liftIO $ atomically $ writeTVar cache newValues

getBowBotAccountByDiscord :: (MonadIOBotData m d r, HasCache BowBotAccount d) => UserId -> m (Maybe BowBotAccount)
getBowBotAccountByDiscord did = find ((did `elem`) . accountDiscords) . HM.elems <$> getCacheMap

getBowBotAccountByMinecraft :: (MonadIOBotData m d r, HasCache BowBotAccount d) => UUID -> m (Maybe BowBotAccount)
getBowBotAccountByMinecraft uuid = find ((uuid `elem`) . accountMinecrafts) . HM.elems <$> getCacheMap