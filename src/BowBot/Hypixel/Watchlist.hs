{-# LANGUAGE QuasiQuotes #-}

module BowBot.Hypixel.Watchlist where

import BowBot.Minecraft.Basic
import BowBot.Network.Basic
import BowBot.Counter.Basic
import BowBot.Hypixel.Basic
import Control.Concurrent.Async (mapConcurrently)
import BowBot.Utils
import BowBot.DB.Typed
import BowBot.Minecraft.Account


getWatchlist :: (MonadIOReader m r, Has Connection r) => m [UUID]
getWatchlist = queryLogT [mysql|SELECT `minecraft_uuid` FROM `watchlist`|]

getWatchlistAccounts :: (MonadIOReader m r, Has Connection r) => m [MinecraftAccount]
getWatchlistAccounts = queryLogT [mysql|SELECT MinecraftAccount FROM `minecraft` JOIN `watchlist` ON `minecraft_uuid`=`minecraft`.`uuid`|]

clearOnlinePlayers :: (MonadIOReader m r, Has Connection r) => m ()
clearOnlinePlayers = void $ executeLogT [mysql|UPDATE `watchlist` SET `online` = NULL|]

getOnlinePlayers :: (MonadIOReader m r, HasAll [Connection, Manager, CounterState] r) => m (Maybe [UUID])
getOnlinePlayers = do
  unknownPlayers :: [UUID] <- queryLogT [mysql|SELECT `minecraft_uuid` FROM `watchlist` WHERE `online` IS NULL|]
  unless (null unknownPlayers) $ do
    ctx <- ask
    cv <- tryIncreaseCounter HypixelApi (fromIntegral $ length unknownPlayers)
    res' <- case cv of
      Nothing -> fmap Just $ liftIO $ flip mapConcurrently unknownPlayers $ \uuid -> flip runReaderT ctx $ do
        inBowDuels <- isInBowDuels uuid
        pure (uuid, inBowDuels)
      _ -> return Nothing
    case res' of
      Nothing -> pure ()
      Just res -> void $ executeLogT [mysql|INSERT INTO `watchlist` (`minecraft_uuid`, ^`online`) VALUES res..|]
  Just <$> queryLogT [mysql|SELECT `minecraft_uuid` FROM `watchlist` WHERE `online` = 1|]

isInBowDuels :: (MonadIOReader m r, Has Manager r) => UUID -> m (Maybe Bool)
isInBowDuels uuid = hypixelWithPlayerStatus uuid $ \o -> do
  session <- o .: "session"
  mode :: Maybe Text <- session .:? "mode"
  return $ mode == Just "DUELS_BOW_DUEL"