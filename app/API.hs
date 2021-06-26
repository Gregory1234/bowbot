{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module API where

import Network.HTTP.Conduit
import Control.Exception.Base (try, SomeException)
import Data.ByteString.Lazy (ByteString)
import Data.Text (unpack)
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.HashMap.Strict as HM
import Data.Aeson
import System.Environment.Blank (getEnv)
import qualified Data.Vector as V
import Discord.Types
import Stats
import Data.List (intercalate)

managerSettings :: ManagerSettings
managerSettings = tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro 15000000 }

sendRequestTo :: Manager -> String -> IO ByteString
sendRequestTo manager url = do
  putStrLn url
  request <- parseRequest url
  res <- try $ httpLbs request manager
  case res of
    (Left (e :: SomeException)) -> do
      print e
      sendRequestTo manager url
    (Right v) -> return $ responseBody v


minecraftNameToUUID :: Manager -> String -> IO (Maybe String)
minecraftNameToUUID manager name = do
  let url = "https://api.mojang.com/users/profiles/minecraft/" ++ name
  res <- sendRequestTo manager url
  case decode res :: Maybe Object of
    Nothing -> return Nothing
    (Just js) -> do
      putStrLn $ "Received response from: " ++ url
      case js HM.!? "id" of
        (Just (String text)) -> do
          return . Just $ unpack text
        _ -> return Nothing

minecraftUuidToNames :: Manager -> String -> IO [String]
minecraftUuidToNames manager uuid = do
  let url = "https://api.mojang.com/user/profiles/" ++ uuid ++ "/names"
  res <- sendRequestTo manager url
  case decode res :: Maybe [Object] of
    Nothing -> return []
    (Just js) -> do
      putStrLn $ "Received response from: " ++ url
      return . mapMaybe helper $ reverse js
  where
    helper x = case x HM.! "name" of
       (String text) -> Just $ unpack text
       _ -> Nothing

updateMinecraftNames :: Manager -> String -> [String] -> IO ()
updateMinecraftNames manager uuid names = do
  website <- fromMaybe "" <$> getEnv "DB_SITE"
  apiKey <- fromMaybe "" <$> getEnv "DB_KEY"
  let url = "http://" ++ website ++ "/updateMinecraftNames.php?key=" ++ apiKey ++ "&uuid=" ++ uuid ++ "&names=" ++ intercalate "," (reverse names)
  _ <- sendRequestTo manager url
  putStrLn $ "Received response from: " ++ url
  pure ()

isInBowDuels :: Manager -> String -> IO (Maybe Bool)
isInBowDuels manager uuid = do
  apiKey <- fromMaybe "" <$> getEnv "HYPIXEL_API"
  let url = "https://api.hypixel.net/status?key=" ++ apiKey ++ "&uuid=" ++ uuid
  res <- sendRequestTo manager url
  case decode res :: Maybe Object of
    Nothing -> return Nothing
    (Just js) -> do
      putStrLn $ "Received response from: " ++ url
      case js HM.!? "session" of
        (Just (Object ses)) -> case ses HM.!? "mode" of
          (Just (String name)) -> return . Just $ name == "BOW_DUEL"
          _ -> return $ Just False
        _ -> return $ Just False

getHypixelStats :: Manager -> String -> IO (Maybe Stats)
getHypixelStats manager uuid = do
  apiKey <- fromMaybe "" <$> getEnv "HYPIXEL_API"
  let url = "https://api.hypixel.net/player?key=" ++ apiKey ++ "&uuid=" ++ uuid
  res <- sendRequestTo manager url
  case decode res :: Maybe Object of
    Nothing -> return Nothing
    (Just js) -> do
      putStrLn $ "Received response from: " ++ url
      case js HM.!? "player" of
        (Just (Object pl)) -> case (pl HM.!? "displayname", pl HM.!? "stats") of
          (Just (String (unpack -> playerName)), Just (Object st)) -> case st HM.!? "Duels" of
            (Just (Object ds)) -> let
              bowWins = roundNum 0 $ ds HM.!? "bow_duel_wins"
              bowLosses = roundNum 0 $ ds HM.!? "bow_duel_losses"
              currentWinstreak = roundNum 0 $ ds HM.!? "current_bow_winstreak"
              bestWinstreak = roundNum 0 $ ds HM.!? "best_bow_winstreak"
              bestDailyWinstreak = roundNum 0 $ ds HM.!? "duels_winstreak_best_bow_duel"
              bowHits = roundNum 0 $ ds HM.!? "bow_duel_bow_hits"
              bowShots = roundNum 0 $ ds HM.!? "bow_duel_bow_shots"
              in return . Just $ Stats {..}
            _ -> return . Just $ Stats {bowWins = 0, bowLosses = 0, currentWinstreak = 0, bestWinstreak = 0, bestDailyWinstreak = 0, bowHits = 0, bowShots = 0, ..}
          _ -> return Nothing
        _ -> return Nothing
  where
    roundNum _ (Just (Number (round -> x))) = x
    roundNum y _ = y

getWatchlist :: Manager -> IO [String]
getWatchlist manager = do
  website <- fromMaybe "" <$> getEnv "DB_SITE"
  apiKey <- fromMaybe "" <$> getEnv "DB_KEY"
  let url = "http://" ++ website ++ "/watchlist.php?key=" ++ apiKey
  res <- sendRequestTo manager url
  case decode res :: Maybe Object of
    Nothing -> return []
    (Just js) -> do
      putStrLn $ "Received response from: " ++ url
      case js HM.!? "data" of
        (Just (Array (V.toList -> list))) -> return $ mapMaybe str list
        _ -> return []
  where
    str (String (unpack -> d)) = Just d
    str _ = Nothing

getMinecraftNickList :: Manager -> IO [(String, [String])]
getMinecraftNickList manager = do
  website <- fromMaybe "" <$> getEnv "DB_SITE"
  apiKey <- fromMaybe "" <$> getEnv "DB_KEY"
  let url = "http://" ++ website ++ "/autocorrect.php?key=" ++ apiKey
  res <- sendRequestTo manager url
  case decode res :: Maybe Object of
   Nothing -> return []
   (Just js) -> do
     putStrLn $ "Received response from: " ++ url
     case js HM.!? "data" of
       (Just (Array (V.toList -> list))) -> return $ mapMaybe person list
       _ -> return []
  where
   person :: Value -> Maybe (String, [String])
   person (Object x) = case (x HM.!? "uuid", x HM.!? "names") of
     (Just (str -> Just uuid), Just (Array (V.toList -> list))) -> Just (uuid, reverse $ mapMaybe str list)
     _ -> Nothing
   person _ = Nothing
   str (String (unpack -> d)) = Just d
   str _ = Nothing

getPeopleSettings :: Manager -> IO [(UserId, StatsSettings)]
getPeopleSettings manager = do
  website <- fromMaybe "" <$> getEnv "DB_SITE"
  apiKey <- fromMaybe "" <$> getEnv "DB_KEY"
  let url = "http://" ++ website ++ "/settings.php?key=" ++ apiKey
  res <- sendRequestTo manager url
  case decode res :: Maybe Object of
    Nothing -> return []
    (Just js) -> do
      putStrLn $ "Received response from: " ++ url
      case js HM.!? "data" of
        (Just (Array (V.toList -> list))) -> return $ mapMaybe parseSettings list
        _ -> return []
  where
    parseSettings (Object js) = let
      discord = maybe 0 read $ str (js HM.!? "discord")
      sWins = parseBool (js HM.!? "wins")
      sLosses = parseBool (js HM.!? "losses")
      sWLR = parseSense (js HM.!? "wlr")
      sWinsUntil = parseSense (js HM.!? "winsUntil")
      sBestStreak = parseBool (js HM.!? "bestStreak")
      sCurrentStreak = parseBool (js HM.!? "currentStreak")
      sBestDailyStreak = parseBool (js HM.!? "bestDailyStreak")
      sBowHits = parseBool (js HM.!? "bowHits")
      sBowShots = parseBool (js HM.!? "bowShots")
      sAccuracy = parseSense (js HM.!? "accuracy")
      in Just (discord, StatsSettings {..})
    parseSettings _ = Nothing
    parseBool (Just "yes") = True
    parseBool (Just "no") = False
    parseBool _ = True
    parseSense (Just "always") = Always
    parseSense (Just "sensibly") = WhenSensible
    parseSense (Just "never") = Never
    parseSense _ = Always
    str (Just (String (unpack -> d))) = Just d
    str _ = Nothing

getPeopleSelectedAccounts :: Manager -> IO [(Integer, [UserId], String, [String])]
getPeopleSelectedAccounts manager = do
  website <- fromMaybe "" <$> getEnv "DB_SITE"
  apiKey <- fromMaybe "" <$> getEnv "DB_KEY"
  let url = "http://" ++ website ++ "/people.php?key=" ++ apiKey
  res <- sendRequestTo manager url
  case decode res :: Maybe Object of
   Nothing -> return []
   (Just js) -> do
     putStrLn $ "Received response from: " ++ url
     case js HM.!? "data" of
       (Just (Object (HM.toList -> list))) -> return $ mapMaybe parsePerson list
       _ -> return []
  where
   parsePerson (read . unpack -> gid, Object js) = let
         discord = map read $ strlist (js HM.!? "discord")
         selected = fromMaybe "" $ str (js HM.!? "selected")
         uuid = strlist (js HM.!? "minecraft")
         in Just (gid, discord, selected, uuid)
   parsePerson _ = Nothing
   str :: Maybe Value -> Maybe String
   str (Just (String (unpack -> d))) = Just d
   str _ = Nothing
   strlist :: Maybe Value -> [String]
   strlist (Just (Array (V.toList -> d))) = mapMaybe (str . Just) d
   strlist _ = []