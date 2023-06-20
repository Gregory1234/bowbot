module BowBot.Account.Register where

import BowBot.Minecraft.Basic (UUID(..))
import Discord.Internal.Rest (UserId)
import BowBot.BotData.Cached
import BowBot.DB.Basic
import BowBot.Utils
import BowBot.Account.Basic
import Control.Monad.Except (runExceptT, throwError)

createNewBowBotAccount :: (MonadIOBotData m d r, HasCache BowBotAccount d, Has Connection r) => Text -> UserId -> UUID -> m (Maybe BowBotAccount)
createNewBowBotAccount name did uuid = do
  cache <- getCache
  r <- ask
  ret <- liftIO $ flip runReaderT r $ withTransaction $ (either (const $ rollback $> Nothing) (pure . Just) =<<) $ runExceptT $ do
    c1 <- executeLog "INSERT INTO `people`(`name`, `roles`, `birthday`) SELECT ?, `roles`, `birthday` FROM `unregistered` WHERE `discord` = ?" (name, did)
    when (c1 <= 0) $ throwError ()
    bid <- BowBotId <$> insertID
    void $ executeLog "DELETE FROM `unregistered` WHERE `discord` = ?" (Only did)
    c2 <- executeLog "INSERT INTO `peopleMinecraft`(`id`, `minecraft`,`status`, `selected`, `verified`) VALUES (?,?, 'main', 1, 0)" (bid, uuid)
    when (c2 <= 0) $ throwError ()
    c3 <- executeLog "INSERT INTO `peopleDiscord`(`id`, `discord`) VALUES (?,?)" (bid, did)
    when (c3 <= 0) $ throwError ()
    pure $ BowBotAccount { accountBotId = bid, accountDiscords = [did], accountSelectedMinecraft = uuid, accountMinecrafts = [uuid] }
  liftIO $ atomically $ for_ ret $ \bacc -> modifyTVar cache (insertMany [(accountBotId bacc, bacc)])
  return ret

addAltToBowBotAccount :: (MonadIOBotData m d r, HasCache BowBotAccount d) => BowBotId -> UUID -> m (Maybe BowBotAccount)
addAltToBowBotAccount bid uuid = do
  acc <- fromJust <$> getFromCache bid
  success <- liftIO $ withDB $ \conn -> (>0) <$> executeLog' conn "INSERT INTO `peopleMinecraft`(`id`, `minecraft`,`status`, `selected`, `verified`) VALUES (?,?, 'alt', 0, 0)" (bid, uuidString uuid)
  let newacc = acc { accountMinecrafts = accountMinecrafts acc ++ [uuid] }
  when success $ do
    cache <- getCache
    liftIO $ atomically $ modifyTVar cache (insertMany [(bid, newacc)])
  return $ if success then Just newacc else Nothing