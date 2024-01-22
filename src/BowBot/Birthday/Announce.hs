{-# LANGUAGE QuasiQuotes #-}

module BowBot.Birthday.Announce where

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
import Data.Bifunctor (first)
import qualified Data.Text as T

birthdayChannelInfo :: InfoType ChannelId
birthdayChannelInfo = InfoType { infoName = "birthday_channel", infoDefault = 0, infoParse = first pack . readEither . unpack }

announceBirthdays :: (MonadIOReader m r, HasAll [DiscordHandle, Connection, InfoCache] r) => m ()
announceBirthdays = do
  currentDay <- liftIO currentBirthdayDate
  birthdays <- getBirthdaysByDate currentDay
  birthdayChannel <- askInfo birthdayChannelInfo
  dcMembers <- HM.fromList . map (\x -> (discordId x, x)) <$> getDiscordGuildMemberAccounts
  logInfoFork $ "Announcing birthdays: " <> T.intercalate ", " (map (showDiscordName . discordName) . filter discordIsMember . mapMaybe (dcMembers HM.!?) $ birthdays)
  pns :: HM.HashMap UserId BowBotId <- HM.fromList <$> queryLog [mysql|SELECT `discord_id`, `account_id` FROM `account_discord`|]
  let (registered, unregistered) = partition (isJust . (pns HM.!?)) birthdays
  let peopleMap = M.toList $ M.filter (not . null) $ M.map (filter discordIsMember . mapMaybe (dcMembers HM.!?)) $ groupByToMap (pns HM.!) registered
  for_ peopleMap $ \(_, p) -> call $ R.CreateMessage birthdayChannel $ "**Happy birthday** to " <> T.intercalate ", " (map (showDiscordNameDiscord . discordName) p) <> "!"
  let unregisteredMap = filter discordIsMember . mapMaybe (dcMembers HM.!?) $ unregistered
  for_ unregisteredMap $ \p -> call $ R.CreateMessage birthdayChannel $ "**Happy birthday** to " <> (showDiscordNameDiscord . discordName) p <> "!"