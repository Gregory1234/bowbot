module BowBot.Account.Register where

import BowBot.Minecraft.Basic (UUID(..))
import Discord.Internal.Rest (UserId)
import BowBot.BotData.Cached
import BowBot.DB.Basic
import BowBot.Utils
import BowBot.Account.Basic
import Control.Monad.Except (runExceptT, throwError)

createNewBowBotAccount :: (MonadIOReader m r, Has Connection r) => Text -> UserId -> UUID -> m (Maybe BowBotId)
createNewBowBotAccount name did uuid = do
  r <- ask
  liftIO $ flip runReaderT r $ withTransaction $ (either (const $ rollback $> Nothing) (pure . Just) =<<) $ runExceptT $ do
    c1 <- executeLog "INSERT INTO `people`(`name`, `roles`, `birthday`) SELECT ?, `roles`, `birthday` FROM `unregistered` WHERE `discord` = ?" (name, did)
    when (c1 <= 0) $ throwError ()
    bid <- BowBotId <$> insertID
    void $ executeLog "DELETE FROM `unregistered` WHERE `discord` = ?" (Only did)
    c2 <- executeLog "INSERT INTO `peopleMinecraft`(`id`, `minecraft`,`status`, `selected`, `verified`) VALUES (?,?, 'main', 1, 0)" (bid, uuid)
    when (c2 <= 0) $ throwError ()
    c3 <- executeLog "INSERT INTO `peopleDiscord`(`id`, `discord`) VALUES (?,?)" (bid, did)
    when (c3 <= 0) $ throwError ()
    pure bid

addAltToBowBotAccount :: (MonadIOReader m r, Has Connection r) => BowBotId -> UUID -> m Bool
addAltToBowBotAccount bid uuid = (>0) <$> executeLog "INSERT INTO `peopleMinecraft`(`id`, `minecraft`,`status`, `selected`, `verified`) VALUES (?,?, 'alt', 0, 0)" (bid, uuidString uuid)