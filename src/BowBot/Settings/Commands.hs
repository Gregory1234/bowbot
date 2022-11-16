{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.Settings.Commands where

import BowBot.Command
import BowBot.Settings.Basic
import BowBot.Discord.Utils
import BowBot.BotData.Cached

boolArg :: Text -> Maybe Bool
boolArg "yes" = Just True
boolArg "show" = Just True
boolArg "always" = Just True
boolArg "no" = Just False
boolArg "hide" = Just False
boolArg "never" = Just False
boolArg _ = Nothing

senseArg :: Text -> Maybe BoolSense
senseArg "yes" = Just Always
senseArg "show" = Just Always
senseArg "always" = Just Always
senseArg "no" = Just Never
senseArg "hide" = Just Never
senseArg "never" = Just Never
senseArg "maybe" = Just WhenSensible
senseArg "defined" = Just WhenSensible
senseArg _ = Nothing

wrongSettingValueMessage :: Text
wrongSettingValueMessage = "*Wrong setting value!*"

wrongSettingNameMessage :: Text
wrongSettingNameMessage = "*Wrong setting name!*"

successfullyUpdatedMessage :: Text
successfullyUpdatedMessage = "*Successfully updated!*"

settingHasThatValueAlreadyMessage :: Text
settingHasThatValueAlreadyMessage = "*The setting has this value already!*"

setSettingCommand :: Command
setSettingCommand = Command CommandInfo
  { commandName = "set"
  , commandHelpEntries = [HelpEntry { helpUsage = "set [stat] [yes|always|show|no|never|hide|maybe|defined] ", helpDescription = "sets the visibility of the stat", helpGroup = "settings" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ twoArguments (\sn val -> liftMaybe wrongSettingNameMessage (getSingleSettingByName sn) >>= (\case SingleSettingBool get set -> (\v -> (flip set v, (== v) . get)) <$> liftMaybe wrongSettingValueMessage (boolArg val); SingleSettingSense get set -> (\v -> (flip set v, (== v) . get)) <$> liftMaybe wrongSettingValueMessage (senseArg val) )) $ \(set, check) -> do
    sender <- userId <$> envs envSender
    setting <- fromMaybe defSettings <$> getFromCache sender
    if check setting
      then respond settingHasThatValueAlreadyMessage
      else do
        a <- storeInCacheIndexed [(sender, set setting)]
        respond $ if a then successfullyUpdatedMessage else somethingWentWrongMessage

constSettingCommand :: Bool -> BoolSense -> Text -> Text -> Command
constSettingCommand boolVal senseVal name desc = Command CommandInfo
  { commandName = name
  , commandHelpEntries = [HelpEntry { helpUsage = name <> " [stat]", helpDescription = desc, helpGroup = "settings" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneArgument (\sn -> (\case SingleSettingBool get set -> (flip set boolVal, (== boolVal) . get); SingleSettingSense get set -> (flip set senseVal, (== senseVal) . get)) <$> liftMaybe wrongSettingNameMessage (getSingleSettingByName sn)) $ \(set, check) -> do
    sender <- userId <$> envs envSender
    setting <- fromMaybe defSettings <$> getFromCache sender
    if check setting
      then respond settingHasThatValueAlreadyMessage
      else do
        a <- storeInCacheIndexed [(sender, set setting)]
        respond $ if a then successfullyUpdatedMessage else somethingWentWrongMessage