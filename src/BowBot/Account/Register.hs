module BowBot.Account.Register where

import BowBot.Minecraft.Basic (UUID(..))
import Discord.Internal.Rest (UserId)
import BowBot.DB.Basic
import BowBot.Utils
import BowBot.Account.Basic
import Control.Monad.Except (runExceptT, throwError)

getOrCreateDummyBowBotAccount :: (MonadIOReader m r, Has Connection r) => UserId -> m (Maybe BowBotId)
getOrCreateDummyBowBotAccount did = do
  bid' <- getBowBotIdByDiscord did
  case bid' of
    Just _ -> return bid'
    Nothing -> createDummyBowBotAccount did

createDummyBowBotAccount :: (MonadIOReader m r, Has Connection r) => UserId -> m (Maybe BowBotId)
createDummyBowBotAccount did = do
  r <- ask
  liftIO $ flip runReaderT r $ withTransaction $ (either (const $ rollback $> Nothing) (pure . Just) =<<) $ runExceptT $ do
    c1 <- executeLog_ "INSERT INTO `account`(`name`) VALUES (\"Dummy Account\")"
    when (c1 <= 0) $ throwError ()
    bid <- BowBotId <$> insertID
    c3 <- executeLog "INSERT INTO `account_discord`(`account_id`,`discord_id`) VALUES (?,?)" (bid, did)
    when (c3 <= 0) $ throwError ()
    pure bid

addFirstMinecraftAccount :: (MonadIOReader m r, Has Connection r) => BowBotId -> Text -> UUID -> m Bool
addFirstMinecraftAccount bid name uuid = do
  _ <- executeLog "UPDATE `account` SET `name` = ? WHERE `id` = ?" (name, bid)
  (>0) <$> executeLog "INSERT INTO `account_minecraft`(`account_id`,`minecraft_uuid`,`type`,`selected`,`verified`) VALUES (?,?, 'main', 1, 0)" (bid, uuid)

addAltToBowBotAccount :: (MonadIOReader m r, Has Connection r) => BowBotId -> UUID -> m Bool
addAltToBowBotAccount bid uuid = (>0) <$> executeLog "INSERT INTO `account_minecraft`(`account_id`,`minecraft_uuid`,`type`,`selected`,`verified`) VALUES (?,?, 'alt', 0, 0)" (bid, uuidString uuid)