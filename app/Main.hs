{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative ((<|>))
import Control.Concurrent (forkFinally, forkIO, threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM
import Control.Monad (forever, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import Data.Text (pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Discord
import qualified Discord.Requests as R
import Discord.Types
import Network.HTTP.Conduit
import System.Environment.Blank (getEnv)
import Data.Char (isDigit)
import Stats
import Utils
import API
import Commands
import Data.List.Split (chunksOf)
import Data.Traversable (for)



main :: IO ()
main = do
  apiKey <- fromMaybe "" <$> getEnv "API_KEY"
  unless (apiKey == "") $ do
    count <- atomically $ newTVar 0
    countBorder <- atomically $ newTVar 0
    nickCache <- atomically $ newTVar []
    online <- atomically $ newTVar Nothing
    onlineBorder <- atomically $ newTVar Nothing
    onlineBusy <- atomically $ newTVar False
    peopleSettings <- atomically $ newTVar []
    peopleNicks <- atomically $ newTVar []
    let bbdata = BowBotData {..}
    updateData bbdata
    mkBackground bbdata
    manager <- newManager managerSettings
    forever $ do
      userFacingError <-
        runDiscord $
          def
            { discordToken = pack apiKey,
              discordOnEvent = eventHandler bbdata manager
            }
      TIO.putStrLn userFacingError

updateData :: BowBotData -> IO ()
updateData BowBotData {..} = do
  manager <- newManager managerSettings
  _ <- forkIO $ updateNicks manager nickCache
  _ <- forkIO $ updateSettings manager peopleSettings peopleNicks
  pure ()

updateNicks :: Manager -> TVar [(String, [String])] -> IO ()
updateNicks manager nickCache = do
  nickList <- getFullNickUUIDList manager
  let chunked = chunksOf 10 nickList
  updatedNicks <- fmap concat $ for chunked $ mapConcurrently (\u -> (u,) <$> uuidToNames manager u)
  atomically $ writeTVar nickCache updatedNicks

updateSettings :: Manager -> TVar [(UserId, StatsSettings)] -> TVar [(UserId, String)] -> IO ()
updateSettings manager peopleSettings peopleNicks = do
  settings <- getAllSettings manager
  nicks <- getDiscordNicks manager
  atomically $ writeTVar peopleSettings settings
  atomically $ writeTVar peopleNicks nicks

mkBackground :: BowBotData -> IO ()
mkBackground bbdata = void $
  forkFinally (background bbdata) $ \e -> do
    print e
    mkBackground bbdata

background :: BowBotData -> IO ()
background bbdata@BowBotData {..} = do
  sec <- read @Int <$> getTime "%S"
  threadDelay ((65 - sec `mod` 60) * 1000000)
  forever go
  where
    go = do
      _ <- forkIO $ do
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
        when (mint == "00") $ updateData bbdata
        putStrLn "New minute finished!"
      threadDelay 60000000



eventHandler :: BowBotData -> Manager -> Event -> DiscordHandler ()
eventHandler dt@BowBotData {..} sm event = case event of
  MessageCreate m -> do
    unless (fromBot m) $ case unpack $ T.toLower . T.takeWhile (/= ' ') $ messageText m of
      "?s" -> commandTimeout 12 $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        settings <- liftIO $ atomically $ readTVar peopleSettings
        statsCommand dt sm (fromMaybe defSettings $ lookup (userId $ messageAuthor m) settings) m
      "?sd" -> commandTimeout 12 $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        statsCommand dt sm defSettings m
      "?sa" -> commandTimeout 12 $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        statsCommand dt sm allSettings m
      "?online" -> commandTimeout 20 $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        t <- liftIO $ read @Int <$> getTime "%S"
        cv <- liftIO . atomically $ do
          c <- readTVar onlineBusy
          unless c $ writeTVar onlineBusy True
          return (not c)
        if cv
          then do
            onlo <- liftIO . atomically $ readTVar online
            onlb <- liftIO . atomically $ readTVar online
            manager <- liftIO $ newManager managerSettings
            o <- case onlo <|> onlb of
              (Just onl) -> do
                _ <- restCall . R.CreateMessage (messageChannel m) $ "**Players in wachList currently in bow duels:** (cached response)"
                pure onl
              Nothing -> do
                people <- liftIO $ getWatchlist manager
                status <- liftIO $ mapConcurrently (\u -> (u,) . fromMaybe False <$> isInBowDuels manager u) people
                let onl = map fst $ filter snd status
                liftIO . atomically $ writeTVar (if t <= 5 || t >= 55 then onlineBorder else online) $ Just onl
                _ <- restCall . R.CreateMessage (messageChannel m) $ "**Players in wachList currently in bow duels:**"
                pure onl
            liftIO . atomically $ writeTVar onlineBusy False
            names <- liftIO $ traverse (fmap last . uuidToNames' manager nickCache) o
            let msg = if null names then "None of the watchListed players are currently in bow duels." else pack . unlines . map (" - " ++) $ names
            _ <- restCall . R.CreateMessage (messageChannel m) $ "```" <> msg <> "```"
            pure ()
          else do
            _ <- restCall . R.CreateMessage (messageChannel m) . pack $ "**Processing list of online players. Please send command again later.**"
            pure ()
      "?list" -> commandTimeout 2 $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        manager <- liftIO $ newManager managerSettings
        st <- liftIO $ getWatchlist manager
        people <- traverse (liftIO . uuidToNames' manager nickCache) st
        let str = T.unwords . map pack . mapMaybe listToMaybe $ people
        _ <- restCall . R.CreateMessage (messageChannel m) $ "**Players in wachList:**" <> "```\n" <> str <> "```"
        pure ()
      "?help" -> commandTimeout 2 $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        _ <-
          restCall . R.CreateMessage (messageChannel m) $
            "**Bow bot help:**\n\n"
              <> "**Commands:**\n"
              <> " - **?help** - *display this message*\n"
              <> " - **?online** - *show all people from watchList currently in Bow Duels*\n"
              <> " - **?list** - *show all players in watchList*\n"
              <> " - **?s [name]** - *show player's Bow Duels stats*\n"
              <> " - **?sa [name]** - *show all Bow Duels stats*\n"
              <> " - **?sd [name]** - *show a default set of Bow Duels stats*\n"
              <> "\nMade by **GregC**#9698"
        pure ()
      "?add" -> commandTimeout 2 $ when (isAdmin (messageAuthor m)) $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        manager <- liftIO $ newManager managerSettings
        let wrds = tail $ words $ unpack $ messageText m
        case wrds of
          [did, "u", uuid] -> liftIO $ addNick manager peopleNicks (read (filter isDigit did)) uuid
          [did, "n", name] -> do
            Just uuid <- liftIO $ nameToUUID' manager nickCache name
            liftIO $ addNick manager peopleNicks (read (filter isDigit did)) uuid
          _ -> pure ()
      _ -> pure ()
  _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isAdmin :: User -> Bool
isAdmin user = userId user == 422051538391793675