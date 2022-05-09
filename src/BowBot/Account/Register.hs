{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.Account.Register where

import BowBot.Minecraft.Basic (UUID(..))
import Discord.Internal.Rest (UserId)
import BowBot.BotData.Cached
import BowBot.DB.Basic (withDB, executeLog)
import BowBot.Utils
import BowBot.Account.Basic
import BowBot.Discord.Roles
import BowBot.Birthday.Basic
import Database.MySQL.Simple.Types (Only(..))
import Database.MySQL.Simple (withTransaction, rollback, insertID)
import Data.Functor (($>))
import Control.Monad.Except (runExceptT, throwError)
import Data.List (intercalate)
import Data.Maybe (fromJust)

createNewBowBotAccount :: (MonadIO m, MonadReader r m, HasCache BowBotAccount r, HasCache SavedRoles r, HasCache BirthdayDate r) => String -> UserId -> UUID -> m (Maybe BowBotAccount)
createNewBowBotAccount name did uuid = do
  cache <- getCache
  savedRoles <- getFromCache did
  birthday <- getFromCache did
  ret <- liftIO $ withDB $ \conn -> withTransaction conn $ (either (const $ rollback conn $> Nothing) (pure . Just) =<<) $ runExceptT $ do
    void $ executeLog conn "DELETE FROM `unregisteredDEV` WHERE `discord` = ?" (Only (toInteger did))
    c1 <- executeLog conn "INSERT INTO `peopleDEV`(`name`, `roles`, `birthday`) VALUES (?,?,?)" (name, maybe "" (intercalate "," . getSavedRoleNames) savedRoles, birthdayString <$> birthday)
    when (c1 <= 0) $ throwError ()
    bid <- liftIO $ fromIntegral <$> insertID conn
    c2 <- executeLog conn "INSERT INTO `peopleMinecraftDEV`(`id`, `minecraft`,`status`, `selected`, `verified`) VALUES (?,?, 'main', 1, 0)" (bid, uuidString uuid)
    when (c2 <= 0) $ throwError ()
    c3 <- executeLog conn "INSERT INTO `peopleDiscordDEV`(`id`, `discord`) VALUES (?,?)" (bid, toInteger did)
    when (c3 <= 0) $ throwError ()
    pure $ BowBotAccount { accountId = bid, accountDiscords = [did], accountSelectedMinecraft = uuid, accountMinecrafts = [uuid] }
  liftIO $ atomically $ for_ ret $ \bacc -> modifyTVar cache (insertMany [(accountId bacc, bacc)])
  return ret

addAltToBowBotAccount :: (MonadIO m, MonadReader r m, HasCache BowBotAccount r) => Integer -> UUID -> m (Maybe BowBotAccount)
addAltToBowBotAccount bid uuid = do
  acc <- fromJust <$> getFromCache bid
  success <- liftIO $ withDB $ \conn -> (>0) <$> executeLog conn "INSERT INTO `peopleMinecraftDEV`(`id`, `minecraft`,`status`, `selected`, `verified`) VALUES (?,?, 'alt', 0, 0)" (bid, uuidString uuid)
  let newacc = acc { accountMinecrafts = accountMinecrafts acc ++ [uuid] }
  when success $ do
    cache <- getCache
    liftIO $ atomically $ modifyTVar cache (insertMany [(bid, newacc)])
  return $ if success then Just newacc else Nothing