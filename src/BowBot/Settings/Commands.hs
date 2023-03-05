module BowBot.Settings.Commands where

import BowBot.Command
import BowBot.Settings.Basic
import BowBot.Discord.Utils
import BowBot.Settings.Arg

setSettingCommand :: Command
setSettingCommand = Command CommandInfo
  { commandName = "set"
  , commandHelpEntries = [HelpEntry { helpUsage = "set [stat] [yes|always|show|no|never|hide|maybe|defined] ", helpDescription = "sets the visibility of the stat", helpGroup = "settings" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ twoArguments $ \sn val -> do
    (set, check) <- settingValArg sn val
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
  } $ oneArgument $ \sn -> do
    (set, check) <- settingArg sn (Just binVal) (Just terVal)
    sender <- userId <$> envs envSender
    settings <- getSettingsByDiscord sender
    if check settings
      then respond settingHasThatValueAlreadyMessage
      else do
        a <- setSettingsByDiscord sender (set settings)
        respond $ if a then successfullyUpdatedMessage else somethingWentWrongMessage