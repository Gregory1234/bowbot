{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module BowBot.Account.Register where

import BowBot.Minecraft.Basic (UUID(..))
import Discord.Internal.Rest (UserId)
import BowBot.BotData.Cached
import BowBot.DB.Basic
import BowBot.Utils
import BowBot.Account.Basic
import BowBot.Discord.Roles
import BowBot.Birthday.Basic
import Control.Monad.Except (runExceptT, throwError)

createNewBowBotAccount :: (MonadIOBotData m d r, HasCaches [BowBotAccount, SavedRoles, BirthdayDate] d, Has Connection r) => String -> UserId -> UUID -> m (Maybe BowBotAccount)
createNewBowBotAccount name did uuid = do
  cache <- getCache
  savedRoles <- getFromCache did
  birthday <- getFromCache did
  r <- ask
  ret <- liftIO $ flip runReaderT r $ withTransaction $ (either (const $ rollback $> Nothing) (pure . Just) =<<) $ runExceptT $ do
    void $ executeLog "DELETE FROM `unregisteredDEV` WHERE `discord` = ?" (Only (toInteger did))
    c1 <- executeLog "INSERT INTO `peopleDEV`(`name`, `roles`, `birthday`) VALUES (?,?,?)" (name, maybe "" (intercalate "," . getSavedRoleNames) savedRoles, birthdayString <$> birthday)
    when (c1 <= 0) $ throwError ()
    bid <- insertID
    c2 <- executeLog "INSERT INTO `peopleMinecraftDEV`(`id`, `minecraft`,`status`, `selected`, `verified`) VALUES (?,?, 'main', 1, 0)" (bid, uuidString uuid)
    when (c2 <= 0) $ throwError ()
    c3 <- executeLog "INSERT INTO `peopleDiscordDEV`(`id`, `discord`) VALUES (?,?)" (bid, toInteger did)
    when (c3 <= 0) $ throwError ()
    pure $ BowBotAccount { accountId = bid, accountDiscords = [did], accountSelectedMinecraft = uuid, accountMinecrafts = [uuid] }
  liftIO $ atomically $ for_ ret $ \bacc -> modifyTVar cache (insertMany [(accountId bacc, bacc)])
  return ret

addAltToBowBotAccount :: (MonadIOBotData m d r, HasCache BowBotAccount d) => Integer -> UUID -> m (Maybe BowBotAccount)
addAltToBowBotAccount bid uuid = do
  acc <- fromJust <$> getFromCache bid
  success <- liftIO $ withDB $ \conn -> (>0) <$> executeLog' conn "INSERT INTO `peopleMinecraftDEV`(`id`, `minecraft`,`status`, `selected`, `verified`) VALUES (?,?, 'alt', 0, 0)" (bid, uuidString uuid)
  let newacc = acc { accountMinecrafts = accountMinecrafts acc ++ [uuid] }
  when success $ do
    cache <- getCache
    liftIO $ atomically $ modifyTVar cache (insertMany [(bid, newacc)])
  return $ if success then Just newacc else Nothing