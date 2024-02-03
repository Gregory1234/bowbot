{-# LANGUAGE QuasiQuotes #-}

module BowBot.Account.Register where

import BowBot.Minecraft.Basic (UUID(..))
import Discord.Internal.Rest (UserId)
import BowBot.DB.Basic
import BowBot.Utils
import BowBot.Account.Basic
import Control.Monad.Except (runExceptT, throwError)

getOrCreateDummyBowBotAccount :: (MonadIOReader m r, Has SafeMysqlConn r) => UserId -> m (Maybe BowBotId)
getOrCreateDummyBowBotAccount did = do
  bid' <- getBowBotIdByDiscord did
  case bid' of
    Just _ -> return bid'
    Nothing -> createDummyBowBotAccount did

createDummyBowBotAccount :: (MonadIOReader m r, Has SafeMysqlConn r) => UserId -> m (Maybe BowBotId)
createDummyBowBotAccount did = do
  r <- ask
  liftIO $ flip runReaderT r $ withTransaction $ (either (const $ rollback $> Nothing) (pure . Just) =<<) $ runExceptT $ do
    bid <- liftMaybe () =<< executeIDLog [mysql|INSERT AI INTO `account`(`name`) VALUES ("Dummy Account")|]
    c3 <- executeLog [mysql|INSERT INTO `account_discord`(`account_id`,`discord_id`) VALUES (bid, did)|]
    when (c3 <= 0) $ throwError ()
    pure bid

addFirstMinecraftAccount :: (MonadIOReader m r, Has SafeMysqlConn r) => BowBotId -> Text -> UUID -> m Bool
addFirstMinecraftAccount bid name uuid = do
  _ <- executeLog [mysql|UPDATE `account` SET `name` = name WHERE `id` = bid|]
  (>0) <$> executeLog [mysql|INSERT INTO `account_minecraft`(`account_id`,`minecraft_uuid`,`type`,`selected`,`verified`) VALUES (bid, uuid, 'main', 1, 0)|]

addAltToBowBotAccount :: (MonadIOReader m r, Has SafeMysqlConn r) => BowBotId -> UUID -> m Bool
addAltToBowBotAccount bid uuid = (>0) <$> executeLog [mysql|INSERT INTO `account_minecraft`(`account_id`,`minecraft_uuid`,`type`,`selected`,`verified`) VALUES (bid,uuid, 'alt', 0, 0)|]