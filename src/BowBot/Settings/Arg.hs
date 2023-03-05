module BowBot.Settings.Arg where

import BowBot.Utils
import BowBot.Settings.Basic
import Control.Monad.Error.Class

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

settingAnyArg :: Text -> (Maybe SettingBin, Maybe SettingTer)
settingAnyArg val = (settingBinArg val, settingTerArg val)

wrongSettingValueMessage :: Text
wrongSettingValueMessage = "*Wrong setting value!*"

wrongSettingNameMessage :: Text
wrongSettingNameMessage = "*Wrong setting name!*"

successfullyUpdatedMessage :: Text
successfullyUpdatedMessage = "*Successfully updated!*"

settingHasThatValueAlreadyMessage :: Text
settingHasThatValueAlreadyMessage = "*The setting has this value already!*"

settingArg :: (MonadIOReader m r, MonadError Text m) => Text -> Maybe SettingBin -> Maybe SettingTer -> m (Settings -> Settings, Settings -> Bool)
settingArg val bin ter = do
  setting <- liftMaybe wrongSettingNameMessage (getSingleSettingByName val)
  case setting of
    SingleSettingBin get set -> do
      bin' <- liftMaybe wrongSettingValueMessage bin
      return (flip set bin', (== bin') . get)
    SingleSettingTer get set -> do
      ter' <- liftMaybe wrongSettingValueMessage ter
      return (flip set ter', (== ter') . get)

settingValArg :: (MonadIOReader m r, MonadError Text m) => Text -> Text -> m (Settings -> Settings, Settings -> Bool)
settingValArg sn val = let (bin, ter) = settingAnyArg val in settingArg sn bin ter 