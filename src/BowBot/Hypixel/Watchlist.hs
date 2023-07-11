module BowBot.Hypixel.Watchlist where

import BowBot.Minecraft.Basic
import BowBot.Network.Basic
import BowBot.Counter.Basic
import BowBot.Hypixel.Api
import Control.Concurrent.Async (mapConcurrently)
import BowBot.Utils
import BowBot.DB.Basic
import BowBot.Minecraft.Account


getWatchlist :: (MonadIOReader m r, Has Connection r) => m [UUID]
getWatchlist = queryLog_ "SELECT `minecraft_uuid` FROM `watchlist`"

getWatchlistAccounts :: (MonadIOReader m r, Has Connection r) => m [MinecraftAccount]
getWatchlistAccounts = queryLog_ "SELECT `minecraft`.`uuid`, `minecraft`.`names` FROM `minecraft` JOIN `watchlist` ON `watchlist`.`minecraft_uuid`=`minecraft`.`uuid`"

clearOnlinePlayers :: (MonadIOReader m r, Has Connection r) => m ()
clearOnlinePlayers = void $ executeLog_ "UPDATE `watchlist` SET `online` = NULL"

getOnlinePlayers :: (MonadIOReader m r, HasAll [Connection, Manager, CounterState] r) => m (Maybe [UUID])
getOnlinePlayers = do
  unknownPlayers :: [UUID] <- queryLog_ "SELECT `minecraft_uuid` FROM `watchlist` WHERE `online` IS NULL"
  unless (null unknownPlayers) $ do
    ctx <- ask
    cv <- tryIncreaseCounter HypixelApi (fromIntegral $ length unknownPlayers)
    res <- case cv of
      Nothing -> fmap Just $ liftIO $ flip mapConcurrently unknownPlayers $ \uuid -> flip runReaderT ctx $ do
        inBowDuels <- isInBowDuels uuid
        pure (uuid, inBowDuels)
      _ -> return Nothing
    case res of
      Nothing -> pure ()
      Just res' -> void $ executeManyLog "INSERT INTO `watchlist` (`minecraft_uuid`, `online`) VALUES (?, ?) ON DUPLICATE KEY UPDATE `online`=VALUES(`online`)" res'
  Just <$> queryLog_ "SELECT `minecraft_uuid` FROM `watchlist` WHERE `online` = 1"

isInBowDuels :: (MonadIOReader m r, Has Manager r) => UUID -> m (Maybe Bool)
isInBowDuels uuid = hypixelWithPlayerStatus uuid $ \o -> do
  session <- o .: "session"
  mode :: Maybe Text <- session .:? "mode"
  return $ mode == Just "DUELS_BOW_DUEL"