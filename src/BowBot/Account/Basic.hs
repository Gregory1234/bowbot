{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.Account.Basic where

import BowBot.Minecraft.Basic (UUID(..))
import Discord.Internal.Rest (UserId)
import BowBot.BotData.Cached
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Proxy (Proxy(..))
import BowBot.DB.Basic (queryLog)
import BowBot.Utils
import Database.MySQL.Simple (Only(..))
import Data.List (find)

data BowBotAccount = BowBotAccount
  { accountId :: Integer
  , accountDiscords :: [UserId]
  , accountSelectedMinecraft :: UUID
  , accountMinecrafts :: [UUID]
  } deriving (Show, Eq)

instance Cached BowBotAccount where
  type CacheIndex BowBotAccount = Integer
  refreshCache conn _ = do
    cache <- getCache (Proxy @BowBotAccount)
    ids :: [Only Integer] <- queryLog conn "SELECT `id` FROM `peopleDEV`" ()
    minecrafts :: [(Integer, String, Bool)] <- queryLog conn "SELECT `id`, `minecraft`, `selected` FROM `peopleMinecraftDEV`" ()
    discords :: [(Integer, Integer)] <- queryLog conn "SELECT `id`, `discord` FROM `peopleDiscordDEV`" ()
    let minecraftsMap = M.map (\l -> let u = map (\(_,b,c) -> (UUID b,c)) l in (fst $ head $ filter snd u, map fst u)) $ groupByToMap (\(a,_,_) -> a) minecrafts
    let discordsMap = M.map (map (fromIntegral . snd)) $ groupByToMap fst discords
    let newValues = HM.fromList $ flip fmap ids $ \(Only i) -> (i, let (accountSelectedMinecraft, accountMinecrafts) = minecraftsMap M.! i in BowBotAccount { accountId = i, accountDiscords = discordsMap M.! i, ..})
    liftIO $ atomically $ writeTVar cache newValues
  storeInCacheIndexed _ = undefined -- TODO
    {-assertGoodIndexes accs
    success <- liftIO $ withDB $ \conn -> do
      minecrafts :: [(Integer, String, Bool)] <- queryLog conn "SELECT `id`, `minecraft`, `selected` FROM `peopleMinecraftDEV` WHERE `id` IN ?" (Only (In (map fst accs)))
      discords :: [(Integer, Integer)] <- queryLog conn "SELECT `id`, `discord` FROM `peopleDiscordDEV` WHERE `id` IN ?" (Only (In (map fst accs)))
      let minecraftsMap = M.map (\l -> let u = map (\(_,b,c) -> (b,c)) l in (fst $ head $ filter snd u, map fst u)) $ groupByToMap (\(a,_,_) -> a) minecrafts
      let discordsMap = M.map (map (fromIntegral . snd)) $ groupByToMap fst discords
      let addedMinecrafts = accs >>= (\(_, BowBotAccount {..}) -> ())
      return False
    let toQueryParams (_, BowBotAccount {..}) = (uuidString mcUUID, head mcNames, intercalate "," mcNames, isBannedToString mcHypixelBow)
    success <- liftIO $ withDB $ \conn -> (== fromIntegral (length accs)) <$> executeManyLog conn "INSERT INTO `minecraftDEV` (`uuid`, `name`, `names`, `hypixel`) VALUES (?,?,?,?) ON DUPLICATE KEY UPDATE `name`=VALUES(`name`), `names`=VALUES(`names`), `hypixel`=VALUES(`hypixel`)" (map toQueryParams accs)
    when success $ do
      cache <- getCache (Proxy @BowBotAccount)
      liftIO $ atomically $ modifyTVar cache (insertMany accs)
    return success
    -}

getBowBotAccountByDiscord :: MonadCache BowBotAccount m => UserId -> m (Maybe BowBotAccount)
getBowBotAccountByDiscord did = find ((did `elem`) . accountDiscords) . HM.elems <$> getCacheMap (Proxy @BowBotAccount)

instance CachedIndexed BowBotAccount where
  cacheIndex = accountId