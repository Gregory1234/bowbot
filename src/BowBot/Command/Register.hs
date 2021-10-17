{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module BowBot.Command.Register where

import BowBot.Command
import BowBot.Minecraft
import BowBot.BotData
import Control.Monad.Cont (liftIO, unless)
import Control.Concurrent.STM (atomically, readTVar, writeTVar)
import Network.HTTP.Conduit (Manager)
import Data.Maybe (fromMaybe)
import Discord.Types hiding (accountId)
import Data.Aeson.Types (unexpected, Value(..), parseMaybe, (.:))
import Data.Text (pack, unpack)
import Text.Read (readMaybe)
import BowBot.API
import Data.Aeson (decode)
import Data.Foldable (for_)
import Data.Char (isSpace)
import Data.List (intercalate)

-- TODO: check if creation was successful

addMinecraftAccount :: Manager -> String -> [String] -> IO (Maybe MinecraftAccount)
addMinecraftAccount manager uuid names = do
  _ <- sendDB manager "minecraft/new.php" ["uuid=" ++ uuid, "names=" ++ intercalate "," names]
  return $ Just MinecraftAccount { mcUUID = uuid, mcNames = names }

addAccount :: Manager -> String -> UserId -> String -> IO (Maybe BowBotAccount)
addAccount manager name did uuid = do
  res <- sendDB manager "people/new.php" ["name=" ++ name, "discord=" ++ show did, "verified=0", "minecraft=" ++ uuid]
  let parser = parseMaybe $ \o -> do
        bid <- o .: "id"
        maybe (unexpected (String (pack bid))) return (readMaybe bid)
  return $ case decode res >>= parser of
    Nothing -> Nothing
    Just bid -> Just BowBotAccount { accountId = bid, accountDiscords = [did], accountMinecrafts = [uuid], accountSelectedMinecraft = uuid}

addAltAccount :: Manager -> Integer -> String -> IO ()
addAltAccount manager gid uuid = do
  _ <- sendDB manager "people/alt.php" ["id=" ++ show gid, "verified=0", "minecraft=" ++ uuid]
  return ()

registerCommand :: String -> Bool -> Bool -> Command
registerCommand name isalt isself = Command name 2 $ \m man bdt -> do
  let args = words $ dropWhile isSpace $ dropWhile (not . isSpace) $ unpack (messageText m)
  let (did, mcname) = if isself then (userId $ messageAuthor m, head args) else (read $ head args, args !! 1)
  maybeUUID <- liftIO $ mcNameToUUID man bdt mcname
  case maybeUUID of
    Nothing -> respond m "*The player doesn't exist!*"
    Just uuid -> do
      nicks <- liftIO $ atomically $ readTVar (minecraftAccounts bdt)
      taken <- fmap (>>=accountMinecrafts) $ liftIO $ atomically $ readTVar (bowBotAccounts bdt)
      if uuid `elem` taken
      then respond m "*That account already belongs to someone else!*"
      else do
        names <- liftIO $ fromMaybe [] <$> mcUUIDToNames man bdt uuid
        unless (uuid `elem` map mcUUID nicks) $ do
          newMc <- liftIO $ addMinecraftAccount man uuid names
          for_ newMc $ \x -> liftIO $ atomically $ writeTVar (minecraftAccounts bdt) (x:nicks)
        if isalt
        then do
          pns <- fmap (>>=(\BowBotAccount {..} -> (, accountId) <$> accountDiscords)) $ liftIO $ atomically $ readTVar (bowBotAccounts bdt)
          case lookup did pns of
            Nothing -> respond m "*Somehing went wrong*"
            Just gid -> do
              liftIO $ addAltAccount man gid uuid
              psa <- liftIO $ atomically $ readTVar (bowBotAccounts bdt)
              let oldAcc = head $ filter ((==gid) . accountId) psa
              liftIO $ atomically $ writeTVar (bowBotAccounts bdt) (oldAcc { accountMinecrafts = uuid:accountMinecrafts oldAcc } : filter ((/= gid) . accountId) psa)
              respond m "*Registered successfully*"
        else do
          newAcc <- liftIO $ addAccount man (head names) did uuid
          case newAcc of
            Nothing -> respond m "*Somehing went wrong*"
            Just newAcc' -> do
              psa <- liftIO $ atomically $ readTVar (bowBotAccounts bdt)
              liftIO $ atomically $ writeTVar (bowBotAccounts bdt) (newAcc':psa)
              respond m "*Registered successfully*"