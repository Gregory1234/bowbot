module BowBot.Birthday.Basic where

import BowBot.BotData.Cached
import BowBot.Discord.Utils
import BowBot.DB.Basic
import BowBot.Account.Basic
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as BS
import Database.MySQL.Simple (Param, Result, ToField(..), FromField(..))
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
  acc <- getBowBotIdByDiscord did
  void $ case acc of
    Nothing -> executeLog "INSERT INTO `unregistered` (`discord`, `birthday`) VALUES (?,?) ON DUPLICATE KEY UPDATE `birthday`=VALUES(`birthday`)" (did, bd)
    Just a -> executeLog "INSERT INTO `people` (`id`, `birthday`) VALUES (?,?) ON DUPLICATE KEY UPDATE `birthday`=VALUES(`birthday`)" (a, bd)

getBirthdaysByDate :: (MonadIOReader m r, Has Connection r) => BirthdayDate -> m [UserId]
getBirthdaysByDate date = do
  res :: [Only UserId] <- queryLog "SELECT `discord` FROM `unregistered` WHERE `birthday` = ? UNION SELECT `peopleDiscord`.`discord` FROM `peopleDiscord` JOIN `people` ON `people`.`id`=`peopleDiscord`.`id` WHERE `birthday` = ?" (date, date)
  return $ map fromOnly res

getBirthdayByDiscord :: (MonadIOReader m r, Has Connection r) => UserId -> m (Maybe BirthdayDate)
getBirthdayByDiscord discord = do
  res :: [Only (Maybe BirthdayDate)] <- queryLog "SELECT `birthday` FROM `unregistered` WHERE `discord` = ? UNION SELECT `birthday` FROM `peopleDiscord` JOIN `people` ON `people`.`id`=`peopleDiscord`.`id` WHERE `peopleDiscord`.`discord` = ?" (discord, discord)
  return $ case res of
    [Only (Just date)] -> Just date
    _ -> Nothing