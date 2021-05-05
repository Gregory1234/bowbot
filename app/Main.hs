{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Concurrent.MVar
import Control.Monad (when, void, forever, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text, isPrefixOf, pack, toLower, unpack, strip)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Discord
import qualified Discord.Requests as R
import Discord.Types
import Network.HTTP.Conduit
import System.Environment.Blank (getEnv)
import Data.Ratio ((%))
import Control.Concurrent (forkIO, threadDelay, forkOS, forkFinally)
import Data.Time.Clock
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Foldable (toList)
import Data.List (intercalate)
import System.Timeout (timeout)

main :: IO ()
main = do
  count <- newMVar 0
  currentUpdate <- newMVar 0
  f <- readFile "people.txt"
  online <- newMVar $ (,False) <$> lines f
  mkBackground count online currentUpdate
  apiKey <- fromMaybe "" <$> getEnv "API_KEY"
  userFacingError <-
    runDiscord $
      def
        { discordToken = pack apiKey,
          discordOnEvent = eventHandler count online
        }
  TIO.putStrLn userFacingError

mkBackground :: MVar Int -> MVar [(String, Bool)] -> MVar Int -> IO ()
mkBackground count online currentUpdate = void $ forkFinally (background count online currentUpdate) $ \e -> do
  print e
  mkBackground count online currentUpdate

setAt :: Int -> a -> [a] -> [a]
setAt i a ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 (_:xs) = a : xs
    go n (x:xs) = x : go (n-1) xs
    go _ []     = []
{-# INLINE setAt #-}


background :: MVar Int -> MVar [(String, Bool)] -> MVar Int -> IO ()
background count online currentUpdate = forever go
  where
    go = do
      _ <- timeout 1000000 $ do
        time <- getCurrentTime
        let f = formatTime defaultTimeLocale "%S" time
        when (f == "00") $ putMVar count 0
        modifyMVar_ currentUpdate $ \cu ->
          modifyMVar online $ \onl -> do
            let (updated,_) = onl !! cu
            b <- isInBowDuels updated
            pure (setAt cu (updated, b) onl, if cu == length onl - 1 then 0 else cu + 1)
      threadDelay 1000000

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
  res <- simpleHttp url
  case decode res :: Maybe [Object] of
    Nothing -> return Nothing
    (Just js) -> do
      case last js HM.! "name" of
        (String text) -> do
          return . Just $ unpack text
        _ -> do
          return Nothing

eventHandler :: MVar Int -> MVar [(String, Bool)] -> Event -> DiscordHandler ()
eventHandler count online event = case event of
  MessageCreate m -> do
    unless (fromBot m) $ case unpack $ T.takeWhile (/=' ') $ messageText m of
      "?s" -> do
        cv <- liftIO $ readMVar count
        if cv < 15
          then do
            let name = unpack . strip . T.drop 2 $ messageText m
            uuid' <- liftIO $ nameToUUID name
            case uuid' of
              Nothing -> do
                _ <- restCall $ R.CreateMessage (messageChannel m) "*The player doesn't exist!*"
                pure ()
              (Just uuid) -> do
                stats <- liftIO $ getStats uuid
                _ <- restCall . R.CreateMessage (messageChannel m) . maybe "*The player doesn't exist!*" (pack . showStats) $ stats
                liftIO $ modifyMVar_ count (pure . (+ 1))
                pure ()
          else do
            time <- liftIO getCurrentTime
            let f = formatTime defaultTimeLocale "%S" time
            _ <- restCall . R.CreateMessage (messageChannel m) . pack $ "**Too many requests! Wait another " ++ show (60-read f :: Int) ++ " seconds!**"
            pure ()
      "?online" -> do
        st <- liftIO $ readMVar online
        people <- traverse (liftIO . uuidToName . fst) . filter snd $ st
        let msg = if null people then "```None of the watchListed players are currently in bow duels.```" else "**WatchListed players currently in bow duels are:**```\n" <> (T.unlines . map (pack . (" - " ++)) . catMaybes) people <> "```"
        _ <- restCall $ R.CreateMessage (messageChannel m) msg
        pure ()
      "?list" -> do
        st <- liftIO $ readMVar online
        people <- traverse (liftIO . uuidToName . fst) st
        let str = T.unwords . map pack . catMaybes $ people
        _ <- restCall . R.CreateMessage (messageChannel m) $ "**Players in wachList:**" <> "```\n" <> str <> "```"
        pure ()
      "?help" -> do
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
