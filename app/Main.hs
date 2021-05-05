{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}

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

main :: IO ()
main = do
  count <- atomically $ newTVar 0
  f <- readFile "people.txt"
  online <- atomically $ newTVar $ (,False) <$> lines f
  nickCache <- atomically $ newTVar $ (,"") <$> lines f
  mkBackground 0 (length $ lines f) count online nickCache
  apiKey <- fromMaybe "" <$> getEnv "API_KEY"
  unless (apiKey == "") . forever $ do
    userFacingError <-
      runDiscord $
        def
          { discordToken = pack apiKey,
            discordOnEvent = eventHandler count online nickCache
          }
    TIO.putStrLn userFacingError

mkBackground :: Int -> Int -> TVar Int -> TVar [(String, Bool)] -> TVar [(String, String)] -> IO ()
mkBackground cu maxu count online nickCache = void $ forkFinally (background cu maxu count online nickCache) $ \e -> do
  print e
  mkBackground cu maxu count online nickCache

setAt :: Int -> a -> [a] -> [a]
setAt i a ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 (_:xs) = a : xs
    go n (x:xs) = x : go (n-1) xs
    go _ []     = []
{-# INLINE setAt #-}


background :: Int -> Int -> TVar Int -> TVar [(String, Bool)] -> TVar [(String, String)] -> IO ()
background startu maxu count online nickCache = go startu
  where
    go cu = do
      _ <- timeout 1000000 . forkIO $ do
        time <- getCurrentTime
        let f = formatTime defaultTimeLocale "%S" time
        when (f == "00") . void . forkIO $ do
                putStrLn "new minute!"
                atomically $ writeTVar count 0
                putStrLn "new minute done!"
        b <- isInBowDuels =<< atomically do
          onl <- readTVar online
          return . fst $ onl !! cu
        uname <- uuidToName =<< atomically do
          onl <- readTVar online
          return . fst $ onl !! cu
        atomically $ do
          onl <- readTVar online
          let (updated,_) = onl !! cu
          writeTVar online $ setAt cu (updated, b) onl
          nc <- readTVar nickCache
          case uname of
            (Just n) -> writeTVar nickCache $ setAt cu (updated, n) nc
            Nothing -> pure ()
      threadDelay 1500000
      go (if cu == maxu - 1 then 0 else cu + 1)

data Stats = Stats
  { playerName :: String,
    bowWins :: Integer,
    bowLosses :: Integer,
    wlRatio :: Rational,
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
    (Just js) -> case js HM.!? "player" of
      (Just (Object pl)) -> case pl HM.!? "stats" of
        (Just (Object st)) -> case st HM.!? "Duels" of
          (Just (Object ds)) -> case (pl HM.!? "displayname", ds HM.!? "current_bow_winstreak", ds HM.!? "best_bow_winstreak", ds HM.!? "bow_duel_wins", ds HM.!? "bow_duel_losses") of
            (Just (String (unpack -> playerName)), Just (Number (round -> currentWinstreak)), Just (Number (round -> bestWinstreak)), Just (Number (round -> bowWins)), Just (Number (round -> bowLosses)))
              -> return . Just $ Stats {wlRatio = if bowLosses == 0 then 0 else bowWins % bowLosses, ..}
            _ -> return Nothing
          _ -> return Nothing
        _ -> return Nothing
      _ -> return Nothing


isInBowDuels :: String -> IO Bool
isInBowDuels uuid = do
  apiKey <- fromMaybe "" <$> getEnv "HYPIXEL_API"
  let url = "https://api.hypixel.net/status?key=" ++ apiKey ++ "&uuid=" ++ uuid
  putStrLn url
  res <- simpleHttp url
  case decode res :: Maybe Object of
    Nothing -> return False
    (Just js) -> case js HM.!? "session" of
      (Just (Object ses)) -> case ses HM.!? "mode" of
        (Just (String name)) -> return $ name == "BOW_DUEL"
        _ -> return False
      _ -> return False

showStats :: Stats -> String
showStats Stats {..} =
  "**" ++ playerName ++":**\n - *Bow Duels Wins:* **"++ show bowWins ++"**\n - *Bow Duels Losses:* **" ++ show bowLosses ++ "**\n - *Bow Duels Win/Loss Ratio:* **" ++ show (fromRational wlRatio :: Double)++ "**\n - *Best Bow Duels winstreak:* **"++ show bestWinstreak ++"**\n - *Current Bow Duels winstreak:* **"++ show currentWinstreak ++ "**"

nameToUUID :: String -> IO (Maybe String)
nameToUUID name = do
  let url = "https://api.mojang.com/users/profiles/minecraft/" ++ name
  putStrLn url
  res <- simpleHttp url
  case decode res :: Maybe Object of
    Nothing -> return Nothing
    (Just js) -> case js HM.!? "id" of
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

eventHandler :: TVar Int -> TVar [(String, Bool)] -> TVar [(String, String)] -> Event -> DiscordHandler ()
eventHandler count online nameCache event = case event of
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
                liftIO . atomically $ modifyTVar count (+ 1)
                pure ()
          else do
            time <- liftIO getCurrentTime
            let f = formatTime defaultTimeLocale "%S" time
            _ <- restCall . R.CreateMessage (messageChannel m) . pack $ "**Too many requests! Wait another " ++ show (60-read f :: Int) ++ " seconds!**"
            pure ()
      "?online" -> do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        st <- liftIO . atomically $ readTVar online
        people <- traverse (liftIO . uuidToName' nameCache . fst) . filter snd $ st
        let msg = if null people then "```None of the watchListed players are currently in bow duels.```" else "**WatchListed players currently in bow duels are:**```\n" <> (T.unlines . map (pack . (" - " ++)) . catMaybes) people <> "```"
        _ <- restCall $ R.CreateMessage (messageChannel m) msg
        pure ()
      "?list" -> do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        st <- liftIO . atomically $ readTVar online
        people <- traverse (liftIO . uuidToName' nameCache . fst) st
        let str = T.unwords . map pack . catMaybes $ people
        _ <- restCall . R.CreateMessage (messageChannel m) $ "**Players in wachList:**" <> "```\n" <> str <> "```"
        pure ()
      "?help" -> do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        _ <- restCall . R.CreateMessage (messageChannel m) $
          "**Bow bot help:**\n" <>
          "**Commands:**\n" <>
          " - **?help** - *display this message*\n" <>
          " - **?online** - *show all people from watchlist currently in Bow Duels (data might not be fully up-to-date depending on amount of players watchListed)*\n"<>
          " - **?list** - *show all players in wathlist*\n"<>
          " - **?s [name]** - *show player's Bow Duels stats*\n"
        pure ()
      _ -> pure ()
  _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

command :: Applicative m => Message -> Text -> m () -> m ()
command m c = when (not (fromBot m) && isCommand c (messageText m))

isCommand :: Text -> Text -> Bool
isCommand c = (c `isPrefixOf`) . toLower
