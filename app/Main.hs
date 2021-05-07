{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (when, void, forever, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text, isPrefixOf, pack, toLower, unpack, strip)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Discord
import qualified Discord.Requests as R
import Discord.Types
import Network.HTTP.Conduit
import System.Environment.Blank (getEnv)
import Data.Ratio ((%))
import Control.Concurrent (forkIO, threadDelay, forkFinally)
import Data.Time.Clock
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.Timeout (timeout)
import Control.Concurrent.STM
import Text.Printf (printf)
import Control.Concurrent.Async (mapConcurrently)
import Control.Applicative ((<|>))

main :: IO ()
main = do
  count <- atomically $ newTVar 0
  countBorder <- atomically $ newTVar 0
  f <- readFile "people.txt"
  nickCache <- atomically $ newTVar $ (,"") <$> lines f
  online <- atomically $ newTVar Nothing
  onlineBorder <- atomically $ newTVar Nothing
  updateNicks nickCache
  mkBackground count countBorder online onlineBorder nickCache
  apiKey <- fromMaybe "" <$> getEnv "API_KEY"
  unless (apiKey == "") . forever $ do
    userFacingError <-
      runDiscord $
        def
          { discordToken = pack apiKey,
            discordOnEvent = eventHandler count countBorder online onlineBorder nickCache
          }
    TIO.putStrLn userFacingError

updateNicks :: TVar [(String, String)] -> IO ()
updateNicks nickCache = do
  updatedNicks <- mapConcurrently (\(u, n) -> (u,) . fromMaybe n <$> uuidToName u) =<< atomically (readTVar nickCache)
  atomically $ writeTVar nickCache updatedNicks

mkBackground :: TVar Int -> TVar Int -> TVar (Maybe [String]) -> TVar (Maybe [String])  -> TVar [(String, String)] -> IO ()
mkBackground count countBorder online onlineBorder nickCache = void $ forkFinally (background count countBorder online onlineBorder nickCache) $ \e -> do
  print e
  mkBackground count countBorder online onlineBorder nickCache

setAt :: Int -> a -> [a] -> [a]
setAt i a ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 (_:xs) = a : xs
    go n (x:xs) = x : go (n-1) xs
    go _ []     = []
{-# INLINE setAt #-}

getTime :: String -> IO String
getTime f = formatTime defaultTimeLocale f <$> getCurrentTime

background :: TVar Int -> TVar Int -> TVar (Maybe [String]) -> TVar (Maybe [String]) -> TVar [(String, String)] -> IO ()
background count countBorder online onlineBorder nickCache = do
    sec <- read @Int <$> getTime "%S"
    threadDelay ((65 - sec `mod` 60) * 1000000)
    forever go
  where
    go = do
      _ <- timeout 1000000 . forkIO $ do
        mint <- getTime "%M"
        putStrLn "New minute!"
        atomically $ do
          bc <- readTVar countBorder
          writeTVar count bc
          writeTVar countBorder 0
        atomically $ do
          onl <- readTVar onlineBorder
          writeTVar online onl
          writeTVar onlineBorder Nothing
        when (mint == "00") $ updateNicks nickCache
        putStrLn "New minute finished!"
      threadDelay 60000000

data Stats = Stats
  { playerName :: String,
    bowWins :: Integer,
    bowLosses :: Integer,
    wlRatio :: Maybe Rational,
    bestWinstreak :: Integer,
    currentWinstreak :: Integer
  }
  deriving (Show)

getStats :: String -> IO (Maybe Stats)
getStats uuid = do
  apiKey <- fromMaybe "" <$> getEnv "HYPIXEL_API"
  let url = "https://api.hypixel.net/player?key=" ++ apiKey ++ "&uuid=" ++ uuid
  putStrLn url
  res <- simpleHttp url
  case decode res :: Maybe Object of
    Nothing -> return Nothing
    (Just js) -> do
     putStrLn $ "Received response from: " ++ url
     case js HM.!? "player" of
      (Just (Object pl)) -> case pl HM.!? "stats" of
        (Just (Object st)) -> case st HM.!? "Duels" of
          (Just (Object ds)) -> case (pl HM.!? "displayname", ds HM.!? "current_bow_winstreak", ds HM.!? "best_bow_winstreak", ds HM.!? "bow_duel_wins", ds HM.!? "bow_duel_losses") of
            (Just (String (unpack -> playerName)), Just (Number (round -> currentWinstreak)), Just (Number (round -> bestWinstreak)), Just (Number (round -> bowWins)), Just (Number (round -> bowLosses)))
              -> return . Just $ Stats {wlRatio = if bowLosses == 0 then Nothing else Just $ bowWins % bowLosses, ..}
            (Just (String (unpack -> playerName)), Just (Number (round -> currentWinstreak)), Just (Number (round -> bestWinstreak)), Just (Number (round -> bowWins)), Nothing)
              -> return . Just $ Stats {wlRatio = Nothing, bowLosses = 0, ..}
            (Just (String (unpack -> playerName)), _, _, Nothing, Just (Number (round -> bowLosses)))
              -> return . Just $ Stats {bowWins = 0, currentWinstreak = 0, bestWinstreak = 0, wlRatio = Just 0, ..}
            _ -> return Nothing
          _ -> return Nothing
        _ -> return Nothing
      _ -> return Nothing


isInBowDuels :: String -> IO (Maybe Bool)
isInBowDuels uuid = do
  apiKey <- fromMaybe "" <$> getEnv "HYPIXEL_API"
  let url = "https://api.hypixel.net/status?key=" ++ apiKey ++ "&uuid=" ++ uuid
  putStrLn url
  res <- simpleHttp url
  case decode res :: Maybe Object of
    Nothing -> return Nothing
    (Just js) -> do
     putStrLn $ "Received response from: " ++ url
     case js HM.!? "session" of
      (Just (Object ses)) -> case ses HM.!? "mode" of
        (Just (String name)) -> return . Just $ name == "BOW_DUEL"
        _ -> return $ Just False
      _ -> return $ Just False

showWL :: Maybe Rational -> String
showWL Nothing = "∞"
showWL (Just r) = printf "%.04f" (fromRational r :: Double)

showStats :: Stats -> String
showStats Stats {..} =
  "**" ++ playerName ++":**\n" ++
  "- *Bow Duels Wins:* **"++ show bowWins ++"**\n"++
  " - *Bow Duels Losses:* **" ++ show bowLosses ++ "**\n"++
  " - *Bow Duels Win/Loss Ratio:* **" ++ showWL wlRatio ++ "**\n"++
  " - *Best Bow Duels winstreak:* **"++ show bestWinstreak ++"**\n"++
  " - *Current Bow Duels winstreak:* **"++ show currentWinstreak ++ "**"

nameToUUID :: String -> IO (Maybe String)
nameToUUID name = do
  let url = "https://api.mojang.com/users/profiles/minecraft/" ++ name
  putStrLn url
  res <- simpleHttp url
  case decode res :: Maybe Object of
    Nothing -> return Nothing
    (Just js) -> do
     putStrLn $ "Received response from: " ++ url
     case js HM.!? "id" of
      (Just (String text)) -> do
        return . Just $ unpack text
      _ -> return Nothing

uuidToName :: String -> IO (Maybe String)
uuidToName uuid = do
  let url = "https://api.mojang.com/user/profiles/" ++ uuid ++ "/names"
  putStrLn url
  res <- simpleHttp url
  case decode res :: Maybe [Object] of
    Nothing -> return Nothing
    (Just js) -> do
      putStrLn $ "Received response from: " ++ url
      case last js HM.! "name" of
        (String text) -> do
          return . Just $ unpack text
        _ -> do
          return Nothing

nameToUUID' :: TVar [(String, String)] -> String -> IO (Maybe String)
nameToUUID' nameCache name = do
  cache <- atomically $ readTVar nameCache
  case filter ((==name) . snd) cache of
    [] -> nameToUUID name
    ((uuid,_):_) -> return $ Just uuid

uuidToName' :: TVar [(String, String)] -> String -> IO (Maybe String)
uuidToName' nameCache uuid = do
  cache <- atomically $ readTVar nameCache
  case filter ((==uuid) . fst) cache of
    [] -> uuidToName uuid
    ((_,name):_) -> return $ Just name

eventHandler :: TVar Int -> TVar Int -> TVar (Maybe [String]) -> TVar (Maybe [String]) -> TVar [(String, String)] -> Event -> DiscordHandler ()
eventHandler count countBorder online onlineBorder nameCache event = case event of
  MessageCreate m -> do
    unless (fromBot m) $ case unpack $ T.takeWhile (/=' ') $ messageText m of
      "?s" -> do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        cv <- liftIO . atomically $ readTVar count
        if cv < 15
          then do
            let name = unpack . strip . T.drop 2 $ messageText m
            uuid' <- liftIO $ nameToUUID' nameCache name
            case uuid' of
              Nothing -> do
                _ <- restCall $ R.CreateMessage (messageChannel m) "*The player doesn't exist!*"
                pure ()
              (Just uuid) -> do
                stats <- liftIO $ getStats uuid
                _ <- restCall . R.CreateMessage (messageChannel m) . maybe "*The player doesn't exist!*" (pack . showStats) $ stats
                t <- liftIO $ read @Int <$> getTime "%S"
                liftIO . atomically $ modifyTVar (if t <= 5 || t >= 55 then countBorder else count) (+ 1)
                pure ()
          else do
            time <- liftIO getCurrentTime
            let f = formatTime defaultTimeLocale "%S" time
            _ <- restCall . R.CreateMessage (messageChannel m) . pack $ "**Too many requests! Wait another " ++ show ((65-read @Int f) `mod` 60) ++ " seconds!**"
            pure ()
      "?online" -> do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        onlo <- liftIO . atomically $ readTVar online
        onlb <- liftIO . atomically $ readTVar online
        o <- case onlo <|> onlb of
          (Just onl) -> pure onl
          Nothing -> do
            _ <- restCall . R.CreateMessage (messageChannel m) $ "**Please wait a couple of seconds...**"
            people <- liftIO $ lines <$> readFile "people.txt"
            status <- liftIO $ traverse (\u -> (u,) . fromMaybe False . fmap (fromMaybe False) <$> timeout 1000000 (isInBowDuels u)) people
            t <- liftIO $ read @Int <$> getTime "%S"
            let onl = map fst $ filter snd status
            liftIO . atomically $ writeTVar (if t <= 5 || t >= 55 then onlineBorder else online) $ Just onl
            pure onl
        names <- liftIO $ traverse (uuidToName' nameCache) o
        let msg = if null names then "None of the watchListed players are currently in bow duels." else pack . unlines . map (" - "++) . catMaybes $ names
        _ <- restCall . R.CreateMessage (messageChannel m) $ "```\n" <> msg <> "```"
        pure ()
      "?list" -> do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        st <- liftIO . atomically $ readTVar nameCache
        people <- traverse (liftIO . uuidToName' nameCache . fst) st
        let str = T.unwords . map pack . catMaybes $ people
        _ <- restCall . R.CreateMessage (messageChannel m) $ "**Players in wachList:**" <> "```\n" <> str <> "```"
        pure ()
      "?help" -> do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        _ <- restCall . R.CreateMessage (messageChannel m) $
          "**Bow bot help:**\n\n" <>
          "**Commands:**\n" <>
          " - **?help** - *display this message*\n" <>
          " - **?online** - *show all people from watchList currently in Bow Duels*\n"<>
          " - **?list** - *show all players in watchList*\n"<>
          " - **?s [name]** - *show player's Bow Duels stats*\n\n"<>
          "Made by **GregC**#9698"
        pure ()
      _ -> pure ()
  _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

command :: Applicative m => Message -> Text -> m () -> m ()
command m c = when (not (fromBot m) && isCommand c (messageText m))

isCommand :: Text -> Text -> Bool
isCommand c = (c `isPrefixOf`) . toLower
