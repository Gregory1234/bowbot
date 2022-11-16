{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

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
import BowBot.Discord.Account
import Data.List (partition)

birthdayChannelInfo :: InfoType ChannelId
birthdayChannelInfo = InfoType { infoName = "birthday_channel", infoDefault = 0, infoParse = readEither }

announceBirthdays :: (MonadIOBotData m d r, Has DiscordHandle r, HasCaches [BirthdayDate, BowBotAccount, DiscordAccount, InfoField] d) => m ()
announceBirthdays = do
  currentDay <- liftIO currentBirthdayDate
  birthdays <- getBirthdayPeople currentDay
  birthdayChannel <- askInfo birthdayChannelInfo
  dcaccounts <- getCacheMap
  logInfoFork $ "Announcing birthdays: " ++ intercalate ", " (map showDiscordAccount . filter discordIsMember . map (dcaccounts HM.!) $ birthdays)
  pns <- HM.fromList . ((\BowBotAccount {..} -> (,accountId) <$> accountDiscords) <=< HM.elems) <$> getCacheMap
  let (registered, unregistered) = partition (isJust . (pns HM.!?)) birthdays
  let peopleMap = M.toList $ M.filter (not . null) $ M.map (filter discordIsMember . map (dcaccounts HM.!)) $ groupByToMap (pns HM.!) registered
  for_ peopleMap $ \(_, p) -> call $ R.CreateMessage birthdayChannel $ pack $ "**Happy birthday** to " ++ intercalate ", " (map showDiscordAccountDiscord p) ++ "!"
  let unregisteredMap = filter discordIsMember . map (dcaccounts HM.!) $ unregistered
  for_ unregisteredMap $ \p -> call $ R.CreateMessage birthdayChannel $ pack $ "**Happy birthday** to " ++ showDiscordAccountDiscord p ++ "!"