module BowBot.Birthday.Basic where

import BowBot.Discord.Utils
import BowBot.DB.Basic
import BowBot.Account.Register
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as BS
import qualified Database.MySQL.Base.Types as T


data BirthdayDate = BirthdayDate { birthdayDay :: !Int, birthdayMonth :: !Int } deriving (Show, Eq)

instance Param BirthdayDate
instance Result BirthdayDate

instance ToField BirthdayDate where
  toField = T.encodeUtf8 . birthdayString

instance FromField BirthdayDate where
  fromField = ([T.String], \case
    (BS.split '.' -> [readMaybe . BS.unpack -> Just birthdayDay, readMaybe . BS.unpack -> Just birthdayMonth]) -> Right BirthdayDate {..}
    _ -> Left "Wrong birthday date")

birthdayString :: BirthdayDate -> Text
birthdayString BirthdayDate {..} = pad' False '0' 2 (showt birthdayDay) <> "." <> pad' False '0' 2 (showt birthdayMonth)

birthdayFromString :: Text -> Maybe BirthdayDate
birthdayFromString str = case T.splitOn "." str of
  [readMaybe . unpack -> Just birthdayDay, readMaybe . unpack -> Just birthdayMonth] -> Just BirthdayDate {..}
  _ -> Nothing

currentBirthdayDate :: IO BirthdayDate
currentBirthdayDate = fromJust . birthdayFromString . pack <$> getTime "%d.%m"

setBirthday :: (MonadIOReader m r, Has Connection r) => UserId -> BirthdayDate -> m ()
setBirthday did bd = do
  bid' <- getOrCreateDummyBowBotAccount did
  for_ bid' $ \bid -> executeLog "INSERT INTO `account` (`id`, `birthday`) VALUES (?,?) ON DUPLICATE KEY UPDATE `birthday`=VALUES(`birthday`)" (bid, bd)

getBirthdaysByDate :: (MonadIOReader m r, Has Connection r) => BirthdayDate -> m [UserId]
getBirthdaysByDate date = map fromOnly <$> queryLog "SELECT `account_discord`.`discord_id` FROM `account_discord` JOIN `account` ON `account`.`id`=`account_discord`.`account_id` WHERE `birthday` = ?" (Only date)

getBirthdayByDiscord :: (MonadIOReader m r, Has Connection r) => UserId -> m (Maybe BirthdayDate)
getBirthdayByDiscord discord = (fromOnly =<<) <$> queryOnlyLog "SELECT `birthday` FROM `account_discord` JOIN `account` ON `account`.`id`=`account_discord`.`account_id` WHERE `account_discord`.`discord_id` = ?" (Only discord)