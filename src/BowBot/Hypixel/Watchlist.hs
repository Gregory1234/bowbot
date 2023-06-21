module BowBot.Hypixel.Watchlist where

import BowBot.BotData.Cached
import BowBot.Minecraft.Basic
import BowBot.Network.Basic
import BowBot.Counter.Basic
import BowBot.Hypixel.Basic
import Control.Concurrent.Async (mapConcurrently)
import BowBot.Utils
import BowBot.DB.Basic
import BowBot.Minecraft.Account


getWatchlist :: (MonadIOReader m r, Has Connection r) => m [UUID]
getWatchlist = map fromOnly <$> queryLog "SELECT `minecraft` FROM `watchlist`" ()

getWatchlistAccounts :: (MonadIOReader m r, Has Connection r) => m [MinecraftAccount]
getWatchlistAccounts = queryLog "SELECT `minecraft`.`uuid`, `minecraft`.`names` FROM `minecraft` JOIN `watchlist` ON `watchlist`.`minecraft`=`minecraft`.`uuid`" ()

clearOnlinePlayers :: (MonadIOReader m r, Has Connection r) => m ()
clearOnlinePlayers = void $ executeLog "UPDATE `watchlist` SET `online` = NULL" ()

getOnlinePlayers :: (MonadIOReader m r, HasAll [Connection, Manager, CounterState] r) => m (Maybe [UUID])
getOnlinePlayers = do
  unknownPlayers :: [UUID] <- map fromOnly <$> queryLog "SELECT `minecraft` FROM `watchlist` WHERE `online` IS NULL" ()
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
      Just res' -> void $ executeManyLog "INSERT INTO `watchlist` (`minecraft`, `online`) VALUES (?, ?) ON DUPLICATE KEY UPDATE `online`=VALUES(`online`)" res'
  Just . map fromOnly <$> queryLog "SELECT `minecraft` FROM `watchlist` WHERE `online` = 1" ()

isInBowDuels :: (MonadIOReader m r, Has Manager r) => UUID -> m (Maybe Bool)
isInBowDuels uuid = hypixelWithPlayerStatus uuid $ \o -> do
  session <- o .: "session"
  mode :: Maybe Text <- session .:? "mode"
  return $ mode == Just "DUELS_BOW_DUEL"