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
import Data.Bifunctor (first)
import qualified Data.Text as T

birthdayChannelInfo :: InfoType ChannelId
birthdayChannelInfo = InfoType { infoName = "birthday_channel", infoDefault = 0, infoParse = first pack . readEither . unpack }

announceBirthdays :: (MonadIOBotData m d r, HasAll [DiscordHandle, Connection] r, HasCaches [BowBotAccount, DiscordAccount, InfoField] d) => m ()
announceBirthdays = do
  currentDay <- liftIO currentBirthdayDate
  birthdays <- getBirthdaysByDate currentDay
  birthdayChannel <- askInfo birthdayChannelInfo
  dcaccounts <- getCacheMap
  logInfoFork $ "Announcing birthdays: " <> T.intercalate ", " (map (showDiscordName . discordName) . filter discordIsMember . map (dcaccounts HM.!) $ birthdays)
  pns <- HM.fromList . ((\BowBotAccount {..} -> (,accountBotId) <$> accountDiscords) <=< HM.elems) <$> getCacheMap
  let (registered, unregistered) = partition (isJust . (pns HM.!?)) birthdays
  let peopleMap = M.toList $ M.filter (not . null) $ M.map (filter discordIsMember . map (dcaccounts HM.!)) $ groupByToMap (pns HM.!) registered
  for_ peopleMap $ \(_, p) -> call $ R.CreateMessage birthdayChannel $ "**Happy birthday** to " <> T.intercalate ", " (map (showDiscordNameDiscord . discordName) p) <> "!"
  let unregisteredMap = filter discordIsMember . map (dcaccounts HM.!) $ unregistered
  for_ unregisteredMap $ \p -> call $ R.CreateMessage birthdayChannel $ "**Happy birthday** to " <> (showDiscordNameDiscord . discordName) p <> "!"