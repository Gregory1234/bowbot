{-# LANGUAGE ViewPatterns #-}

module BowBot.Command.Birthday where

import BowBot.Command
import BowBot.Birthday
import Discord
import qualified Discord.Requests as R
import BowBot.API
import Data.List (intercalate)
import Data.Char (isDigit, isSpace)

birthdayAnnounceCommand :: Command
birthdayAnnounceCommand = Command "bdsay" ModLevel 10 $ \m man bdt -> do
  currentDay <- liftIO currentBirthdayDate
  maybeBirthdays <- liftIO $ getBirthdayPeople man currentDay
  case maybeBirthdays of
    Nothing -> respond m somethingWrongMessage
    Just birthdays -> do
      dgid <- readProp discordGuildId bdt
      people <- fmap (filter ((`elem` birthdays) . userId . memberUser)) <$> call (R.ListGuildMembers dgid R.GuildMembersTiming {R.guildMembersTimingLimit = Just 500, R.guildMembersTimingAfter = Nothing})
      case people of
        Left err -> do
          logError man $ show err
          respond m somethingWrongMessage
        Right ppl -> if null ppl
          then respond m "*Noone has a birthday today!*"
          else do
            respond m $ "Today's birthdays: ```\n" ++ unlines (map (showMemberOrUser False . Right) ppl) ++ "```"
            birthdayChannel <- readProp discordBirthdayChannel bdt
            call_ $ R.CreateMessage birthdayChannel $ pack $ 
              if length ppl == 1 
              then "**Happy birthday** to " ++ showMemberOrUser True (Right $ head ppl) ++ "!" 
              else "**Happy birthday** to: " ++ intercalate ", " (map (showMemberOrUser True . Right) ppl) ++ "!"

birthdaySetCommand :: Command
birthdaySetCommand = Command "bdset" ModLevel 10 $ \m man _ -> do
  let args = words $ dropWhile isSpace $ dropWhile (not . isSpace) $ unpack (messageText m)
  case args of
    [readMaybe . filter isDigit -> Just did, birthdayFromString -> Just bd] -> do
      liftIO $ setBirthday man did bd
      respond m "*Birthday set!*"
    _ -> respond m wrongSyntaxMessage

setBirthday :: Manager -> UserId -> BirthdayDate -> IO ()
setBirthday man did bd = void $ sendDB man "discord/birthday/set.php" ["discord=" ++ show did, "date=" ++ birthdayString bd]