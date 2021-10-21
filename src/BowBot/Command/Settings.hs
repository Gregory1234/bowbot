{-# LANGUAGE ViewPatterns #-}

module BowBot.Command.Settings where

import BowBot.Command
import BowBot.Settings
import BowBot.BotData
import Discord.Types
import Data.Text (unpack)
import Network.HTTP.Conduit (Manager)
import Control.Monad (void)
import BowBot.API
import Control.Monad.IO.Class (liftIO)
import Data.Map (alter)
import Control.Concurrent.STM.TVar (modifyTVar)
import Control.Concurrent.STM (atomically)
import Data.Maybe (fromMaybe)

boolArg :: String -> Maybe Bool
boolArg "yes" = Just True
boolArg "show" = Just True
boolArg "always" = Just True
boolArg "no" = Just False
boolArg "hide" = Just False
boolArg "never" = Just False
boolArg _ = Nothing

fromBool :: Bool -> String
fromBool True = "yes"
fromBool False = "no"

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

fromSense :: BoolSense -> String
fromSense Always = "always"
fromSense Never = "never"
fromSense WhenSensible = "sensibly"

unsafeUpdateSettings :: Manager -> UserId -> String -> String -> IO ()
unsafeUpdateSettings man did key val = void $ sendDB man "discord/settings/update.php" ["discord=" ++ show did, "setting=" ++ key, "value=" ++ val]

boolFromEither :: Either (Bool, BoolSense) String -> Maybe Bool
boolFromEither (Left (b, _)) = Just b
boolFromEither (Right (boolArg -> b)) = b

senseFromEither :: Either (Bool, BoolSense) String -> Maybe BoolSense
senseFromEither (Left (_, s)) = Just s
senseFromEither (Right (senseArg -> s)) = s

getSettingToSet :: String -> Either (Bool, BoolSense) String -> Either String (String, String, Settings -> Settings)
getSettingToSet "wins" (boolFromEither -> Just b) = Right ("wins", fromBool b, \s -> s {sWins = b})
getSettingToSet "losses" (boolFromEither -> Just b) = Right ("losses", fromBool b, \s -> s {sLosses = b})
getSettingToSet "wlr" (senseFromEither -> Just b) = Right ("wlr", fromSense b, \s -> s {sWLR = b})
getSettingToSet "winsuntil" (senseFromEither -> Just b) = Right ("winsUntil", fromSense b, \s -> s {sWinsUntil = b})
getSettingToSet "beststreak" (boolFromEither -> Just b) = Right ("bestStreak", fromBool b, \s -> s {sBestStreak = b})
getSettingToSet "currentstreak" (boolFromEither -> Just b) = Right ("currentStreak", fromBool b, \s -> s {sCurrentStreak = b})
getSettingToSet "bestdailystreak" (boolFromEither -> Just b) = Right ("bestDailyStreak", fromBool b, \s -> s {sBestDailyStreak = b})
getSettingToSet "bowhits" (boolFromEither -> Just b) = Right ("bowHits", fromBool b, \s -> s {sBowHits = b})
getSettingToSet "bowshots" (boolFromEither -> Just b) = Right ("bowShots", fromBool b, \s -> s {sBowShots = b})
getSettingToSet "accuracy" (senseFromEither -> Just b) = Right ("accuracy", fromSense b, \s -> s {sAccuracy = b})
getSettingToSet _ _ = Left "*Wrong command argument!*"

settingsCommand :: String -> Maybe (Bool, BoolSense) -> Command
settingsCommand name values = Command name DefaultLevel 2 $ \m man bdt -> do
  let args = tail $ words (unpack $ messageText m)
  let did = userId $ messageAuthor m
  case (values, args) of
    (Nothing, [setting, value]) -> case getSettingToSet setting (Right value) of
      Left err -> respond m err
      Right (k, v, u) -> do
        liftIO $ unsafeUpdateSettings man did k v
        liftIO $ atomically $ modifyTVar (discordSettings bdt) $ alter (Just . u . fromMaybe defSettings) did
        respond m "*Successfully updated!*"
    (Just bs, [setting]) -> case getSettingToSet setting (Left bs) of
      Left err -> respond m err
      Right (k, v, u) -> do
        liftIO $ unsafeUpdateSettings man did k v
        liftIO $ atomically $ modifyTVar (discordSettings bdt) $ alter (Just . u . fromMaybe defSettings) did
        respond m "*Successfully updated!*"
    _ -> respond m wrongSyntaxMessage