{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BowBot.Command.Birthday where

import BowBot.Command
import BowBot.Birthday
import qualified Discord.Requests as R
import Data.Char (isDigit)
import qualified Data.Map as M
import Data.List (intercalate)

birthdayAnnounceCommand :: Command
birthdayAnnounceCommand = Command "bdsay" ModLevel 10 $ do
  currentDay <- liftIO currentBirthdayDate
  birthdays <- getBirthdayPeople currentDay
  dgid <- hRead discordGuildId
  people <- hDiscord $ fmap (filter ((`elem` birthdays) . userId . memberUser)) <$> call (R.ListGuildMembers dgid R.GuildMembersTiming {R.guildMembersTimingLimit = Just 500, R.guildMembersTimingAfter = Nothing})
  case people of
    Left err -> do
      hLogErrorDB $ show err
      hRespond somethingWrongMessage
    Right ppl -> do
      pns <- fmap (>>=(\BowBotAccount {..} -> (,accountId) <$> accountDiscords)) $ hRead bowBotAccounts
      let peopleMap = M.toList $ groupByToMap (\x -> maybe (Left (userId $ memberUser x)) Right $ lookup (userId $ memberUser x) pns) ppl
      if null ppl
        then hRespond "*Noone has a birthday today!*"
        else do
          hRespond $ "Today's birthdays: ```\n" ++ unlines (map (showMemberOrUser False . Right) ppl) ++ "```"
          birthdayChannel <- hRead discordBirthdayChannel
          hDiscord $ for_ peopleMap $ \(_, p) -> call $ R.CreateMessage birthdayChannel $ pack $ "**Happy birthday** to " ++ intercalate ", " (map (showMemberOrUser True . Right) p) ++ "!"

birthdaySetCommand :: Command
birthdaySetCommand = Command "bdset" ModLevel 10 $ do
  args <- hArgs
  case args of
    [readMaybe . filter isDigit -> Just did, birthdayFromString -> Just bd] -> do
      setBirthday did bd
      hRespond "*Birthday set!*"
    _ -> hRespond wrongSyntaxMessage

setBirthday :: DBMonad m => UserId -> BirthdayDate -> m ()
setBirthday did bd = do
  c :: [Only Integer] <- hQueryLog "SELECT `discord` FROM `peopleDiscordDEV` WHERE `discord` = ?" (Only $ show did)
  void $ if null c
    then hExecuteLog "INSERT INTO `unregisteredDEV`(`discord`, `birthday`) VALUES (?, ?) ON DUPLICATE KEY UPDATE `birthday`=VALUES(`birthday`)" (show did, birthdayString bd) -- TODO: error detection
    else hExecuteLog "UPDATE `peopleDEV` JOIN `peopleDiscordDEV` ON `peopleDEV`.`id`=`peopleDiscordDEV`.`id` SET `peopleDEV`.`birthday` = ? WHERE `peopleDiscordDEV`.`discord` = ?" (birthdayString bd, show did)