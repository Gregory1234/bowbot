{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.Birthday.Basic where

import Data.List.Split (splitOn)
import Data.Maybe (fromJust, mapMaybe)
import BowBot.BotData.Cached
import BowBot.Discord.Utils
import qualified Data.HashMap.Strict as HM
import BowBot.DB.Basic (queryLog, executeManyLog, withDB)
import BowBot.Account.Basic


data BirthdayDate = BirthdayDate { birthdayDay :: Int, birthdayMonth :: Int } deriving (Show, Eq)

birthdayString :: BirthdayDate -> String
birthdayString BirthdayDate {..} = pad' False '0' 2 (show birthdayDay) ++ "." ++ pad' False '0' 2 (show birthdayMonth)

birthdayFromString :: String -> Maybe BirthdayDate
birthdayFromString str = case splitOn "." str of
  [readMaybe -> Just a, readMaybe -> Just b] -> Just $ BirthdayDate a b
  _ -> Nothing

currentBirthdayDate :: IO BirthdayDate
currentBirthdayDate = fromJust . birthdayFromString <$> getTime "%d.%m"

instance Cached BirthdayDate where
  type CacheIndex BirthdayDate = UserId
  refreshCache conn = do
    cache <- getCache
    res :: [(Integer, Maybe String)] <- queryLog conn "SELECT `discord`, `birthday` FROM `unregisteredDEV` UNION SELECT `peopleDiscordDEV`.`discord`, `peopleDEV`.`birthday` FROM `peopleDiscordDEV` JOIN `peopleDEV` ON `peopleDEV`.`id`=`peopleDiscordDEV`.`id`" ()
    let newValues = HM.fromList $ flip mapMaybe res $ \(fromInteger -> did, (>>= birthdayFromString) -> bd) -> (did,) <$> bd
    liftIO $ atomically $ writeTVar cache newValues

setBirthday :: (MonadCache BirthdayDate m, MonadCache BowBotAccount m) => UserId -> BirthdayDate -> m Bool
setBirthday did bd = do
  acc <- getBowBotAccountByDiscord did
  case acc of
    Nothing -> do
      success <- liftIO $ withDB $ \conn -> (>0) <$> executeManyLog conn "INSERT INTO `unregisteredDEV` (`discord`, `birthday`) VALUES (?,?) ON DUPLICATE KEY UPDATE `birthday`=VALUES(`birthday`)" [(toInteger did, birthdayString bd)]
      when success $ do
        cache <- getCache
        liftIO $ atomically $ modifyTVar cache (insertMany [(did, bd)])
      return success
    Just a -> do
      success <- liftIO $ withDB $ \conn -> (>0) <$> executeManyLog conn "INSERT INTO `peopleDEV` (`id`, `birthday`) VALUES (?,?) ON DUPLICATE KEY UPDATE `birthday`=VALUES(`birthday`)" [(BowBot.Account.Basic.accountId a, birthdayString bd)]
      when success $ do
        cache <- getCache
        liftIO $ atomically $ modifyTVar cache (insertMany $ map (,bd) (accountDiscords a))
      return success

getBirthdayPeople :: MonadCache BirthdayDate m => BirthdayDate -> m [UserId]
getBirthdayPeople date = HM.keys . HM.filter (== date) <$> getCacheMap