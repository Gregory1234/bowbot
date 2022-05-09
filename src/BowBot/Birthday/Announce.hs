{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.Birthday.Announce where

import BowBot.BotData.Cached
import BowBot.Birthday.Basic
import qualified Discord.Requests as R
import BowBot.Account.Basic
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import BowBot.Discord.Utils
import BowBot.BotData.Info
import BowBot.DB.Basic
import Control.Monad ((<=<))
import Data.List (intercalate)
import BowBot.Discord.Account

birthdayChannelInfo :: InfoType ChannelId
birthdayChannelInfo = InfoType { infoName = "birthday_channel", infoDefault = 0, infoParse = readEither }

announceBirthdays :: (MonadIO m, MonadReader r m, Has DiscordHandle r, HasCache BirthdayDate r, HasCache BowBotAccount r, HasCache DiscordAccount r, HasCache InfoField r) => m ()
announceBirthdays = do
  currentDay <- liftIO currentBirthdayDate
  birthdays <- getBirthdayPeople currentDay
  birthdayChannel <- hInfoDB birthdayChannelInfo
  dcaccounts <- getCacheMap
  logInfo $ "Announcing birthdays: " ++ intercalate ", " (map (showDiscordAccount . (dcaccounts HM.!)) birthdays)
  pns <- HM.fromList . ((\BowBotAccount {..} -> (,accountId) <$> accountDiscords) <=< HM.elems) <$> getCacheMap
  let peopleMap = M.toList $ groupByToMap (pns HM.!) birthdays
  for_ peopleMap $ \(_, p) -> call $ R.CreateMessage birthdayChannel $ pack $ "**Happy birthday** to " ++ intercalate ", " (map (showDiscordAccountDiscord . (dcaccounts HM.!)) p) ++ "!"