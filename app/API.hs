{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module API where

import Network.HTTP.Conduit
import Control.Exception.Base (try, SomeException)
import Data.ByteString.Lazy (ByteString)
import Data.Text (unpack, pack)
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.HashMap.Strict as HM
import Data.Aeson
import System.Environment.Blank (getEnv)
import qualified Data.Vector as V
import Discord.Types
import Stats
import Data.List (intercalate)
import Text.Read (readMaybe)
import Control.Monad.Cont (void)

data MinecraftAccount = MinecraftAccount
  { mcUUID :: String
  , mcNames :: [String]
  , mcHypixel :: Bool
  } deriving (Show)

data PermissionLevel =
    BanLevel
  | DefaultLevel
  | ModLevel
  | AdminLevel
    deriving (Eq, Ord, Enum, Bounded, Show)

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
updateMinecraftNames manager uuid names =
  void $ sendDB manager "updateMinecraftNames.php" ["uuid=" ++ uuid, "names=" ++ intercalate "," names]

addMinecraftAccount :: Manager -> String -> [String] -> Bool -> IO ()
addMinecraftAccount manager uuid names hypixel =
  void $ sendDB manager "addMinecraftName.php" ["uuid=" ++ uuid, "names=" ++ intercalate "," names, "hypixel=" ++ (if hypixel then "1" else "0")]

addAccount :: Manager -> String -> UserId -> String -> IO (Maybe Integer)
addAccount manager name did uuid = do
  res <- sendDB manager "addPerson.php" ["name=" ++ name, "discord=" ++ show did, "verified=0", "minecraft=" ++ uuid]
  case decode res :: Maybe Object of
    Nothing -> return Nothing
    (Just js) -> do
      case js HM.!? "id" of
          (Just (String (readMaybe . unpack -> Just n))) -> return $ Just n
          _ -> return Nothing

addAltAccount :: Manager -> Integer -> String -> IO ()
addAltAccount manager gid uuid = do
  _ <- sendDB manager "addAlt.php" ["id=" ++ show gid, "verified=0", "minecraft=" ++ uuid]
  return ()

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

getHypixelGuildMembers :: Manager -> String -> IO (Maybe [String])
getHypixelGuildMembers manager guildid = do
  apiKey <- fromMaybe "" <$> getEnv "HYPIXEL_API"
  let url = "https://api.hypixel.net/guild?key=" ++ apiKey ++ "&id=" ++ guildid
  res <- sendRequestTo manager url
  case decode res :: Maybe Object of
    Nothing -> return Nothing
    (Just js) -> do
      putStrLn $ "Received response from: " ++ url
      case js HM.!? "guild" of
        (Just (Object gld)) -> case gld HM.!? "members" of
          (Just (Array (V.toList -> list))) -> return . Just $ mapMaybe personUUID list
          _ -> return Nothing
        _ -> return Nothing
  where
    personUUID (Object pl) = case pl HM.!? "uuid" of
      (Just (String (unpack -> uuid))) -> Just uuid
      _ -> Nothing
    personUUID _ = Nothing

sendDB :: Manager -> String -> [String] -> IO ByteString
sendDB manager path args = do
  website <- fromMaybe "" <$> getEnv "DB_SITE"
  apiKey <- fromMaybe "" <$> getEnv "DB_KEY"
  let url = "http://" ++ website ++ "/" ++ path ++ "?key=" ++ apiKey ++ (('&':) =<< args)
  res <- sendRequestTo manager url
  putStrLn $ "Received response from: " ++ url
  return res

getWatchlist :: Manager -> IO [String]
getWatchlist manager = do
  res <- sendDB manager "watchlist.php" []
  case decode res :: Maybe Object of
    Nothing -> return []
    (Just js) -> case js HM.!? "data" of
      (Just (Array (V.toList -> list))) -> return $ mapMaybe str list
      _ -> return []
  where
    str (String (unpack -> d)) = Just d
    str _ = Nothing

getMinecraftNickList :: Manager -> IO [MinecraftAccount]
getMinecraftNickList manager = do
  res <- sendDB manager "autocorrect.php" []
  case decode res :: Maybe Object of
   Nothing -> return []
   (Just js) -> case js HM.!? "data" of
     (Just (Array (V.toList -> list))) -> return $ mapMaybe person list
     _ -> return []
  where
   person :: Value -> Maybe MinecraftAccount
   person (Object x) = case (x HM.!? "uuid", x HM.!? "names", x HM.!? "hypixel") of
     (Just (str -> Just uuid), Just (Array (V.toList -> list)), Just (str -> Just b)) -> Just MinecraftAccount {mcUUID = uuid, mcNames = mapMaybe str list, mcHypixel = b == "1"}
     _ -> Nothing
   person _ = Nothing
   str (String (unpack -> d)) = Just d
   str _ = Nothing

getMinecraftStatList :: Manager -> IO [(String, Int, Int, Int)]
getMinecraftStatList manager = do
  res <- sendDB manager "leaderboard.php" []
  case decode res :: Maybe Object of
    Nothing -> return []
    (Just js) -> case js HM.!? "data" of
      (Just (Array (V.toList -> list))) -> return $ mapMaybe person list
      _ -> return []
  where
   person :: Value -> Maybe (String, Int, Int, Int)
   person (Object x) = case (x HM.!? "uuid", x HM.!? "bowWins", x HM.!? "bowLosses", x HM.!? "bowWinstreak") of
     (Just (str -> Just uuid), Just (fmap read . str -> Just wins), Just (fmap read . str -> Just losses), Just (fmap read . str -> Just winstreak)) -> Just (uuid, wins, losses, winstreak)
     _ -> Nothing
   person _ = Nothing
   str (String (unpack -> d)) = Just d
   str _ = Nothing

getPeoplePerms :: Manager -> IO [(UserId, PermissionLevel)]
getPeoplePerms manager = do
  res <- sendDB manager "perms.php" []
  case decode res :: Maybe Object of
      Nothing -> return []
      (Just js) -> case js HM.!? "data" of
        (Just (Array (V.toList -> list))) -> return $ mapMaybe parsePerms list
        _ -> return []
    where
      parsePerms (Object js) = let
        discord = maybe 0 read $ str (js HM.!? "id")
        level = parseLevel (js HM.!? "level")
        in Just (discord, level)
      parsePerms _ = Nothing
      parseLevel (Just "ban") = BanLevel
      parseLevel (Just "mod") = ModLevel
      parseLevel (Just "admin") = AdminLevel
      parseLevel _ = DefaultLevel
      str (Just (String (unpack -> d))) = Just d
      str _ = Nothing

getPeopleSettings :: Manager -> IO [(UserId, StatsSettings)]
getPeopleSettings manager = do
  res <- sendDB manager "settings.php" []
  case decode res :: Maybe Object of
    Nothing -> return []
    (Just js) -> case js HM.!? "data" of
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
  res <- sendDB manager "people.php" []
  case decode res :: Maybe Object of
   Nothing -> return []
   (Just js) -> case js HM.!? "data" of
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


getDiscordIds :: Manager -> IO [UserId]
getDiscordIds manager = do
  res <- sendDB manager "discordIds.php" []
  case decode res :: Maybe Object of
     Nothing -> return []
     (Just js) -> case js HM.!? "data" of
       (Just (Array (V.toList -> list))) -> return $ mapMaybe readMaybe $ mapMaybe str list
       _ -> return []
  where
   str :: Value -> Maybe String
   str (String (unpack -> d)) = Just d
   str _ = Nothing

getDiscordRoleDisabledIds :: Manager -> IO [UserId]
getDiscordRoleDisabledIds manager = do
  res <- sendDB manager "discordRoleDisabledIds.php" []
  case decode res :: Maybe Object of
     Nothing -> return []
     (Just js) -> case js HM.!? "data" of
       (Just (Array (V.toList -> list))) -> return $ mapMaybe readMaybe $ mapMaybe str list
       _ -> return []
  where
   str :: Value -> Maybe String
   str (String (unpack -> d)) = Just d
   str _ = Nothing

sendPostDB :: Manager -> String -> Value -> IO ()
sendPostDB manager path dat = do
  website <- fromMaybe "" <$> getEnv "DB_SITE"
  apiKey <- fromMaybe "" <$> getEnv "DB_KEY"
  let url = "http://" ++ website ++ "/" ++ path ++ "?key=" ++ apiKey
  putStrLn url
  initRequest <- parseRequest url
  let request = initRequest { method = "POST", requestBody = RequestBodyLBS (encode dat) }
  void $ try @SomeException $ httpLbs request manager

updateDiscords :: Manager -> [GuildMember] -> [User] -> IO ()
updateDiscords manager mem usr = sendPostDB manager "updateDiscord.php" (object $ map memToObject mem ++ map usrToObject usr)
  where
    memToObject GuildMember {memberUser = memberUser@User {..}, ..} = case memberNick of
      Nothing -> usrToObject memberUser
      Just nick -> pack (show userId) .= object ["name" .= userName, "discriminator" .= userDiscrim, "nickname" .= nick]
    usrToObject User {..} = pack (show userId) .= object ["name" .= userName, "discriminator" .= userDiscrim]


updateStats :: Manager -> [(MinecraftAccount, Maybe Stats)] -> IO ()
updateStats manager stats = sendPostDB manager "updateStats.php" (object $ mapMaybe statsToObject stats)
  where
    statsToObject (MinecraftAccount {..}, Just Stats {..}) = Just $ pack mcUUID .= object ["bowWins" .= bowWins, "bowLosses" .= bowLosses, "bowWinstreak" .= bestWinstreak]
    statsToObject (_, Nothing) = Nothing