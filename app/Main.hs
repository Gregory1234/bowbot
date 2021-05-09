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
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client.Conduit (withManager)

data BowBotData = BowBotData
  { count :: TVar Int,
    countBorder :: TVar Int,
    nickCache :: TVar [(String, String)],
    online :: TVar (Maybe [String]),
    onlineBorder :: TVar (Maybe [String]),
    onlineBusy :: TVar Bool
  }
main :: IO ()
main = do
  count <- atomically $ newTVar 0
  countBorder <- atomically $ newTVar 0
  f <- readFile "people.txt"
  nickCache <- atomically $ newTVar $ (,"") <$> lines f
  online <- atomically $ newTVar Nothing
  onlineBorder <- atomically $ newTVar Nothing
  onlineBusy <- atomically $ newTVar False
  let bbdata = BowBotData {..}
  updateNicks nickCache
  mkBackground bbdata
  apiKey <- fromMaybe "" <$> getEnv "API_KEY"
  unless (apiKey == "") . forever $ do
    userFacingError <-
      runDiscord $
        def
          { discordToken = pack apiKey,
            discordOnEvent = eventHandler bbdata
          }
    TIO.putStrLn userFacingError

updateNicks :: TVar [(String, String)] -> IO ()
updateNicks nickCache = do
  manager <- newManager tlsManagerSettings
  updatedNicks <- mapConcurrently (\(u, n) -> (u,) . fromMaybe n <$> uuidToName manager u) =<< atomically (readTVar nickCache)
  atomically $ writeTVar nickCache updatedNicks

mkBackground :: BowBotData -> IO ()
mkBackground bbdata = void $ forkFinally (background bbdata) $ \e -> do
  print e
  mkBackground bbdata

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

background :: BowBotData -> IO ()
background BowBotData {..} = do
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

sendRequestTo :: Manager -> String -> IO ByteString
sendRequestTo manager url = do
  putStrLn url
  request <- parseRequest url
  res <- httpLbs request manager
  return $ responseBody res


data Stats = Stats
  { playerName :: String,
    bowWins :: Integer,
    bowLosses :: Integer,
    bestWinstreak :: Integer,
    currentWinstreak :: Integer
  }
  deriving (Show)

getStats :: Manager -> String -> IO (Maybe Stats)
getStats manager uuid = do
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
          (Just (Object ds)) -> case (ds HM.!? "current_bow_winstreak", ds HM.!? "best_bow_winstreak", ds HM.!? "bow_duel_wins", ds HM.!? "bow_duel_losses") of
            (Just (Number (round -> currentWinstreak)), Just (Number (round -> bestWinstreak)), Just (Number (round -> bowWins)), Just (Number (round -> bowLosses)))
              -> return . Just $ Stats {..}
            (Just (Number (round -> currentWinstreak)), Just (Number (round -> bestWinstreak)), Just (Number (round -> bowWins)), Nothing)
              -> return . Just $ Stats {bowLosses = 0, ..}
            (_, _, Nothing, Just (Number (round -> bowLosses)))
              -> return . Just $ Stats {bowWins = 0, currentWinstreak = 0, bestWinstreak = 0, ..}
            (_, _, Nothing, Nothing)
              -> return . Just $ Stats {bowWins = 0, bowLosses = 0, currentWinstreak = 0, bestWinstreak = 0, ..}
            _ -> return Nothing
          _ -> return . Just $ Stats {bowWins = 0, bowLosses = 0, currentWinstreak = 0, bestWinstreak = 0, ..}
        _ -> return Nothing
      _ -> return Nothing


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

showStats :: Stats -> String
showStats Stats {..} =
  "**" ++ playerName ++":**\n" ++
  "- *Bow Duels Wins:* **"++ show bowWins ++"**\n"++
  " - *Bow Duels Losses:* **" ++ show bowLosses ++ "**\n"++
  " - *Bow Duels Win/Loss Ratio:* **" ++ winLossRatio ++ "**\n"++
  " - *Bow Duels Wins until " ++ nextWinLossRatio ++ " WLR:* **" ++ winsRemaining ++ "**\n" ++
  " - *Best Bow Duels Winstreak:* **"++ show bestWinstreak ++"**\n"++
  " - *Current Bow Duels Winstreak:* **"++ show currentWinstreak ++ "**"
  where
    winLossRatio 
      | bowWins == 0, bowLosses == 0 = "NaN"
      | bowLosses == 0 = "∞"
      | otherwise = printf "%.04f" (fromRational (bowWins%bowLosses) :: Double)
    nextWinLossRatio
      | bowLosses == 0 = "∞"
      | otherwise = show $ (bowWins `div` bowLosses) + 1
    winsRemaining
      | bowWins == 0, bowLosses == 0 = "1"
      | bowLosses == 0 = "N/A"
      | otherwise = show (bowLosses - (bowWins `mod` bowLosses))

nameToUUID :: Manager -> String -> IO (Maybe String)
nameToUUID manager name = do
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

uuidToName :: Manager -> String -> IO (Maybe String)
uuidToName manager uuid = do
  let url = "https://api.mojang.com/user/profiles/" ++ uuid ++ "/names"
  res <- sendRequestTo manager url
  case decode res :: Maybe [Object] of
    Nothing -> return Nothing
    (Just js) -> do
      putStrLn $ "Received response from: " ++ url
      case last js HM.! "name" of
        (String text) -> do
          return . Just $ unpack text
        _ -> do
          return Nothing

nameToUUID' :: Manager -> TVar [(String, String)] -> String -> IO (Maybe String)
nameToUUID' manager nameCache name = do
  cache <- atomically $ readTVar nameCache
  case filter ((==name) . snd) cache of
    [] -> nameToUUID manager name
    ((uuid,_):_) -> return $ Just uuid

uuidToName' :: Manager -> TVar [(String, String)] -> String -> IO (Maybe String)
uuidToName' manager nameCache uuid = do
  cache <- atomically $ readTVar nameCache
  case filter ((==uuid) . fst) cache of
    [] -> uuidToName manager uuid
    ((_,name):_) -> return $ Just name

eventHandler :: BowBotData -> Event -> DiscordHandler ()
eventHandler BowBotData {..} event = case event of
  MessageCreate m -> do
    unless (fromBot m) $ case unpack $ T.takeWhile (/=' ') $ messageText m of
      "?s" -> do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        t <- liftIO $ read @Int <$> getTime "%S"
        cv <- liftIO . atomically $ do
          c1 <- readTVar count
          c2 <- readTVar countBorder
          let c = c1 + c2
          when (c < 15) $ modifyTVar (if t <= 5 || t >= 55 then countBorder else count) (+1)
          return $ c < 15
        if cv
          then do
            manager <- liftIO $ newManager tlsManagerSettings
            let name = unpack . strip . T.drop 2 $ messageText m
            uuid' <- liftIO $ nameToUUID' manager nickCache name
            case uuid' of
              Nothing -> do
                _ <- restCall $ R.CreateMessage (messageChannel m) "*The player doesn't exist!*"
                pure ()
              (Just uuid) -> do
                stats <- liftIO $ getStats manager uuid
                _ <- restCall . R.CreateMessage (messageChannel m) . maybe "*The player doesn't exist!*" (pack . showStats) $ stats
                pure ()
          else do
            f <- liftIO $ read @Int <$> getTime "%S"
            _ <- restCall . R.CreateMessage (messageChannel m) . pack $ "**Too many requests! Wait another " ++ show ((65-f) `mod` 60) ++ " seconds!**"
            pure ()
      "?online" -> do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        t <- liftIO $ read @Int <$> getTime "%S"
        cv <- liftIO . atomically $ do
          c <- readTVar onlineBusy
          when c $ writeTVar onlineBusy True
          return c
        if cv then do
          onlo <- liftIO . atomically $ readTVar online
          onlb <- liftIO . atomically $ readTVar online
          manager <- liftIO $ newManager tlsManagerSettings
          o <- case onlo <|> onlb of
            (Just onl) -> do
              _ <- restCall . R.CreateMessage (messageChannel m) $ "**Players in wachList currently in bow duels:** (cached response)"
              pure onl
            Nothing -> do
              people <- liftIO $ lines <$> readFile "people.txt"
              status <- liftIO $ mapConcurrently (\u -> (u,) . fromMaybe False <$> isInBowDuels manager u) people
              let onl = map fst $ filter snd status
              liftIO . atomically $ writeTVar (if t <= 5 || t >= 55 then onlineBorder else online) $ Just onl
              _ <- restCall . R.CreateMessage (messageChannel m) $ "**Players in wachList currently in bow duels:**"
              pure onl
          liftIO . atomically $ writeTVar onlineBusy False
          names <- liftIO $ traverse (uuidToName' manager nickCache) o
          let msg = if null names then "None of the watchListed players are currently in bow duels." else pack . unlines . map (" - "++) . catMaybes $ names
          _ <- restCall . R.CreateMessage (messageChannel m) $ "```" <> msg <> "```"
          pure ()
        else do
          _ <- restCall . R.CreateMessage (messageChannel m) . pack $ "**Processing list of online players. Please send command again later.**"
          pure ()
      "?list" -> do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        st <- liftIO . atomically $ readTVar nickCache
        manager <- liftIO $ newManager tlsManagerSettings
        people <- traverse (liftIO . uuidToName' manager nickCache . fst) st
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
