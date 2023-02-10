{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.Settings.Commands where

import BowBot.Command
import BowBot.Settings.Basic
import BowBot.Discord.Utils

settingBinArg :: Text -> Maybe SettingBin
settingBinArg "yes" = Just Yes
settingBinArg "show" = Just Yes
settingBinArg "always" = Just Yes
settingBinArg "no" = Just No
settingBinArg "hide" = Just No
settingBinArg "never" = Just No
settingBinArg _ = Nothing

settingTerArg :: Text -> Maybe SettingTer
settingTerArg "yes" = Just Always
settingTerArg "show" = Just Always
settingTerArg "always" = Just Always
settingTerArg "no" = Just Never
settingTerArg "hide" = Just Never
settingTerArg "never" = Just Never
settingTerArg "maybe" = Just WhenSensible
settingTerArg "defined" = Just WhenSensible
settingTerArg _ = Nothing

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
  } $ twoArguments (\sn val -> liftMaybe wrongSettingNameMessage (getSingleSettingByName sn) >>= (\case SingleSettingBin get set -> (\v -> (flip set v, (== v) . get)) <$> liftMaybe wrongSettingValueMessage (settingBinArg val); SingleSettingTer get set -> (\v -> (flip set v, (== v) . get)) <$> liftMaybe wrongSettingValueMessage (settingTerArg val) )) $ \(set, check) -> do
    sender <- userId <$> envs envSender
    settings <- getSettingsByDiscord sender
    if check settings
      then respond settingHasThatValueAlreadyMessage
      else do
        a <- setSettingsByDiscord sender (set settings)
        respond $ if a then successfullyUpdatedMessage else somethingWentWrongMessage

constSettingCommand :: SettingBin -> SettingTer -> Text -> Text -> Command
constSettingCommand binVal terVal name desc = Command CommandInfo
  { commandName = name
  , commandHelpEntries = [HelpEntry { helpUsage = name <> " [stat]", helpDescription = desc, helpGroup = "settings" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneArgument (\sn -> (\case SingleSettingBin get set -> (flip set binVal, (== binVal) . get); SingleSettingTer get set -> (flip set terVal, (== terVal) . get)) <$> liftMaybe wrongSettingNameMessage (getSingleSettingByName sn)) $ \(set, check) -> do
    sender <- userId <$> envs envSender
    settings <- getSettingsByDiscord sender
    if check settings
      then respond settingHasThatValueAlreadyMessage
      else do
        a <- setSettingsByDiscord sender (set settings)
        respond $ if a then successfullyUpdatedMessage else somethingWentWrongMessage