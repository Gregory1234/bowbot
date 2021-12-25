{-# LANGUAGE ViewPatterns #-}

module BowBot.Command.Birthday where

import BowBot.Command
import BowBot.Birthday
import qualified Discord.Requests as R
import Data.List (intercalate)
import Data.Char (isDigit)

birthdayAnnounceCommand :: Command
birthdayAnnounceCommand = Command "bdsay" ModLevel 10 $ do
  currentDay <- liftIO currentBirthdayDate
  maybeBirthdays <- getBirthdayPeople currentDay
  case maybeBirthdays of
    Nothing -> hRespond somethingWrongMessage
    Just birthdays -> do
      dgid <- hRead discordGuildId
      people <- hDiscord $ fmap (filter ((`elem` birthdays) . userId . memberUser)) <$> call (R.ListGuildMembers dgid R.GuildMembersTiming {R.guildMembersTimingLimit = Just 500, R.guildMembersTimingAfter = Nothing})
      case people of
        Left err -> do
          hLogError $ show err
          hRespond somethingWrongMessage
        Right ppl -> if null ppl
          then hRespond "*Noone has a birthday today!*"
          else do
            hRespond $ "Today's birthdays: ```\n" ++ unlines (map (showMemberOrUser False . Right) ppl) ++ "```"
            birthdayChannel <- hRead discordBirthdayChannel
            hDiscord $ call_ $ R.CreateMessage birthdayChannel $ pack $ 
              if length ppl == 1 
              then "**Happy birthday** to " ++ showMemberOrUser True (Right $ head ppl) ++ "!" 
              else "**Happy birthday** to: " ++ intercalate ", " (map (showMemberOrUser True . Right) ppl) ++ "!"

birthdaySetCommand :: Command
birthdaySetCommand = Command "bdset" ModLevel 10 $ do
  args <- hArgs
  case args of
    [readMaybe . filter isDigit -> Just did, birthdayFromString -> Just bd] -> do
      setBirthday did bd
      hRespond "*Birthday set!*"
    _ -> hRespond wrongSyntaxMessage

setBirthday :: APIMonad m => UserId -> BirthdayDate -> m ()
setBirthday did bd = void $ hSendDB "discord/birthday/set.php" ["discord=" ++ show did, "date=" ++ birthdayString bd]