{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.Account.Register where

import BowBot.Minecraft.Basic (UUID(..))
import Discord.Internal.Rest (UserId)
import BowBot.BotData.Cached
import Data.Proxy (Proxy(..))
import BowBot.DB.Basic (withDB, executeLog)
import BowBot.Utils
import BowBot.Account.Basic
import BowBot.Discord.Roles
import Database.MySQL.Simple.Types (Only(..))
import Database.MySQL.Simple (withTransaction, rollback, insertID)
import Data.Functor (($>))
import Control.Monad.Except (runExceptT, throwError)
import Data.List (intercalate)

createNewBowBotAccount :: (MonadCache BowBotAccount m, MonadCache SavedRoles m) => String -> UserId -> UUID -> m (Maybe BowBotAccount)
createNewBowBotAccount name did uuid = do
  cache <- getCache (Proxy @BowBotAccount)
  savedRoles <- getFromCache (Proxy @SavedRoles) did
  ret <- liftIO $ withDB $ \conn -> withTransaction conn $ (either (const $ rollback conn $> Nothing) (pure . Just) =<<) $ runExceptT $ do
    void $ executeLog conn "DELETE FROM `unregisteredDEV` WHERE `discord` = ?" (Only (toInteger did))
    c1 <- executeLog conn "INSERT INTO `peopleDEV`(`name`, `roles`) VALUES (?,?)" (name, maybe "" (intercalate "," . getSavedRoleNames) savedRoles)
    when (c1 <= 0) $ throwError ()
    bid <- liftIO $ fromIntegral <$> insertID conn
    c2 <- executeLog conn "INSERT INTO `peopleMinecraftDEV`(`id`, `minecraft`,`status`, `selected`, `verified`) VALUES (?,?, 'main', 1, 0)" (bid, uuidString uuid)
    when (c2 <= 0) $ throwError ()
    c3 <- executeLog conn "INSERT INTO `peopleDiscordDEV`(`id`, `discord`) VALUES (?,?)" (bid, toInteger did)
    when (c3 <= 0) $ throwError ()
    pure $ BowBotAccount { accountId = bid, accountDiscords = [did], accountSelectedMinecraft = uuid, accountMinecrafts = [uuid] }
  liftIO $ atomically $ for_ ret $ \bacc -> modifyTVar cache (insertMany [(accountId bacc, bacc)])
  return ret