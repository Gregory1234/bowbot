{-# LANGUAGE QuasiQuotes #-}

module BowBot.Birthday.Basic where

import BowBot.Discord.Utils
import BowBot.DB.Basic
import BowBot.Account.Register
import BowBot.Account.Basic
import qualified Data.Text as T


data BirthdayDate = BirthdayDate { birthdayDay :: !Int, birthdayMonth :: !Int }
  deriving (Show, Eq)
  deriving (ToMysql, FromMysql) via (SimpleValue BirthdayDate)

instance ToMysqlSimple BirthdayDate where
  toMysqlValue = toMysqlValue . birthdayString

instance FromMysqlSimple BirthdayDate where
  fromMysqlValue (birthdayFromString . fromMysqlValue -> Just bd) = bd
  fromMysqlValue _ = error "Wrong birthday date"

birthdayString :: BirthdayDate -> Text
birthdayString BirthdayDate {..} = pad' False '0' 2 (showt birthdayDay) <> "." <> pad' False '0' 2 (showt birthdayMonth)

birthdayFromString :: Text -> Maybe BirthdayDate
birthdayFromString str = case T.splitOn "." str of
  [readMaybe . unpack -> Just birthdayDay, readMaybe . unpack -> Just birthdayMonth] -> Just BirthdayDate {..}
  _ -> Nothing

currentBirthdayDate :: IO BirthdayDate
currentBirthdayDate = fromJust . birthdayFromString . pack <$> getTime "%d.%m"

setBirthday :: (MonadIOReader m r, Has SafeMysqlConn r) => UserId -> BirthdayDate -> m ()
setBirthday did bd = do
  bid' <- getOrCreateDummyBowBotAccount did
  for_ bid' $ \bid -> executeLog [mysql|INSERT INTO `account` (`id`, ^`birthday`) VALUES (bid, bd)|]

getBirthdaysByDate :: (MonadIOReader m r, Has SafeMysqlConn r) => BirthdayDate -> m [UserId]
getBirthdaysByDate date = queryLog [mysql|SELECT `account_discord`.`discord_id` FROM `account_discord` JOIN `account` ON `id`=`account_discord`.`account_id` WHERE `birthday` = date|]

getBirthdayByDiscord :: (MonadIOReader m r, Has SafeMysqlConn r) => UserId -> m (Maybe BirthdayDate)
getBirthdayByDiscord discord = queryOnlyLog [mysql|SELECT `birthday` FROM `account_discord` JOIN `account` ON `id`=`account_discord`.`account_id` WHERE `account_discord`.`discord_id` = discord|]