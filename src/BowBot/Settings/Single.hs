module BowBot.Settings.Single where

import Discord.Types (UserId)
import BowBot.DB.Typed
import qualified Database.MySQL.Base.Types as T

data SettingBin = Yes | No
  deriving (Show, Eq, Ord, Enum)
  deriving (QueryParams, QueryResults) via (SimpleValue SettingBin)

instance Param SettingBin
instance Result SettingBin

instance ToField SettingBin where
  toField Yes = "yes"
  toField No = "no"

instance FromField SettingBin where
  fromField = ([T.Enum, T.String], \case
    "yes" -> Right Yes
    "no" -> Right No
    _ -> Left "Wrong permission level")

data SettingTer = Never | WhenSensible | Always
  deriving (Show, Eq, Ord, Enum)
  deriving (QueryParams, QueryResults) via (SimpleValue SettingTer)

instance Param SettingTer
instance Result SettingTer

instance ToField SettingTer where
  toField Always = "always"
  toField Never = "never"
  toField WhenSensible = "sensibly"

instance FromField SettingTer where
  fromField = ([T.Enum, T.String], \case
    "always" -> Right Always
    "never" -> Right Never
    "sensibly" -> Right WhenSensible
    _ -> Left "Wrong permission level")

onlyIfBin :: SettingBin -> a -> Maybe a
onlyIfBin Yes a = Just a
onlyIfBin No _ = Nothing

onlyIfTer :: SettingTer -> Bool -> a -> Maybe a
onlyIfTer Always _ a = Just a
onlyIfTer Never _ _ = Nothing
onlyIfTer WhenSensible True a = Just a
onlyIfTer WhenSensible False _ = Nothing