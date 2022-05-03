{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module BowBot.Settings.Commands where

import BowBot.Command
import BowBot.Settings.Basic
import BowBot.Discord.Utils (liftMaybe, fromMaybe)
import BowBot.BotData.Cached
import Data.Proxy
import Discord.Types (userId)

boolArg :: String -> Maybe Bool
boolArg "yes" = Just True
boolArg "show" = Just True
boolArg "always" = Just True
boolArg "no" = Just False
boolArg "hide" = Just False
boolArg "never" = Just False
boolArg _ = Nothing

senseArg :: String -> Maybe BoolSense
senseArg "yes" = Just Always
senseArg "show" = Just Always
senseArg "always" = Just Always
senseArg "no" = Just Never
senseArg "hide" = Just Never
senseArg "never" = Just Never
senseArg "maybe" = Just WhenSensible
senseArg "defined" = Just WhenSensible
senseArg _ = Nothing

wrongSettingValueMessage :: String
wrongSettingValueMessage = "*Wrong setting value!*"

wrongSettingNameMessage :: String
wrongSettingNameMessage = "*Wrong setting name!*"

successfullyUpdatedMessage :: String
successfullyUpdatedMessage = "*Successfully updated!*"

settingHasThatValueAlreadyMessage :: String
settingHasThatValueAlreadyMessage = "*The setting has this value already!*"

setSettingCommand :: Command
setSettingCommand = Command CommandInfo
  { commandName = "set"
  , commandUsage = "set [stat] [yes|always|show|no|never|hide|maybe|defined] "
  , commandDescription = "sets the visibility of the stat"
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  , commandGroup = "settings"
  } $ hTwoArguments (\sn val -> liftMaybe wrongSettingNameMessage (getSingleSettingByName sn) >>= (\case SingleSettingBool get set -> (\v -> (flip set v, (== v) . get)) <$> liftMaybe wrongSettingValueMessage (boolArg val); SingleSettingSense get set -> (\v -> (flip set v, (== v) . get)) <$> liftMaybe wrongSettingValueMessage (senseArg val) )) $ \(set, check) -> do
    sender <- userId <$> hEnv envSender
    setting <- fromMaybe defSettings <$> getFromCache (Proxy @Settings) sender
    if check setting
      then hRespond settingHasThatValueAlreadyMessage
      else do
        a <- storeInCacheIndexed [(sender, set setting)]
        hRespond $ if a then successfullyUpdatedMessage else somethingWentWrongMessage

constSettingCommand :: Bool -> BoolSense -> String -> String -> Command
constSettingCommand boolVal senseVal name desc = Command CommandInfo
  { commandName = name
  , commandUsage = name ++ " [stat]"
  , commandDescription = desc
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  , commandGroup = "settings"
  } $ hOneArgument (\sn -> (\case SingleSettingBool get set -> (flip set boolVal, (== boolVal) . get); SingleSettingSense get set -> (flip set senseVal, (== senseVal) . get)) <$> liftMaybe wrongSettingNameMessage (getSingleSettingByName sn)) $ \(set, check) -> do
    sender <- userId <$> hEnv envSender
    setting <- fromMaybe defSettings <$> getFromCache (Proxy @Settings) sender
    if check setting
      then hRespond settingHasThatValueAlreadyMessage
      else do
        a <- storeInCacheIndexed [(sender, set setting)]
        hRespond $ if a then successfullyUpdatedMessage else somethingWentWrongMessage