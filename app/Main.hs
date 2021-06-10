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
import Data.Text.Encoding (encodeUtf8)



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
              discordOnStart = onStartup,
              discordOnEvent = eventHandler bbdata manager
            }
      TIO.putStrLn userFacingError

onStartup :: DiscordHandler ()
onStartup = do
  sendCommand (UpdateStatus $ UpdateStatusOpts {
    updateStatusOptsSince = Nothing, 
    updateStatusOptsGame = Just (Activity {activityName = "try out ?settings command", activityType = ActivityTypeGame, activityUrl = Nothing}),
    updateStatusOptsNewStatus = UpdateStatusOnline,
    updateStatusOptsAFK = False
  })

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
            names <- liftIO $ traverse (fmap head . uuidToNames' manager nickCache) o
            let msg = if null names then "None of the watchListed players are currently in bow duels." else pack . unlines . map (" - " ++) $ names
            _ <- restCall . R.CreateMessage (messageChannel m) $ "```" <> msg <> "```"
            pure ()
          else do
            _ <- restCall . R.CreateMessage (messageChannel m) . pack $ "**Processing list of online players. Please send command again later.**"
            pure ()
      "?list" -> commandTimeout 2 $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        let wrds = tail $ words $ unpack $ messageText m
        ln <- case wrds of
          [] -> do
            manager <- liftIO $ newManager managerSettings
            st <- liftIO $ getWatchlist manager
            people <- traverse (liftIO . uuidToNames' manager nickCache) st
            return $ Just (map pack . mapMaybe listToMaybe $ people, "Players in watchList")
          ["w"] -> do
            manager <- liftIO $ newManager managerSettings
            st <- liftIO $ getWatchlist manager
            people <- traverse (liftIO . uuidToNames' manager nickCache) st
            return $ Just (map pack . mapMaybe listToMaybe $ people, "Players in watchList")
          ["ac"] -> do
            st <- liftIO $ atomically $ readTVar nickCache
            return $ Just (map pack $ mapMaybe (listToMaybe . snd) st, "Players on autocomplete list")
          ["d"] -> do
            manager <- liftIO $ newManager managerSettings
            st <- liftIO $ atomically $ readTVar peopleNicks
            people <- traverse (liftIO . uuidToNames' manager nickCache . snd) st
            return $ Just (map pack . mapMaybe listToMaybe $ people, "Players on discord list")
          _ -> return Nothing
        void $ case ln of
          Nothing -> restCall $ R.CreateMessage (messageChannel m) "*Wrong command syntax*"
          Just (list, name) ->
            if sum (map T.length list) < 1800
              then restCall $ R.CreateMessage (messageChannel m) $ "**" <> name <> ":**\n" <> "```\n" <> T.unwords list <> "```"
              else do
                _ <- restCall $ R.CreateMessage (messageChannel m) $ "**" <> name <> ":**"
                restCall $ R.CreateMessageUploadFile (messageChannel m) "list.txt" $ encodeUtf8 (T.unlines list)
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
              <> " - **?settings** - *display help for settings*\n"
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
      "?refresh" -> commandTimeout 2 $ when (isAdmin (messageAuthor m)) $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        liftIO $ updateData dt
      "?settings" -> commandTimeout 2 $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        _ <-
          restCall . R.CreateMessage (messageChannel m) $
            "**You can now customize the output of ?s command!**\n"
              <> "**Commands:**\n"
              <> " - **?settings** - *display this message*\n"
              <> " - **?show [stat]** - *makes the stat visible*\n"
              <> " - **?hide [stat]** - *makes the stat hidden*\n"
              <> " - **?show [stat] [yes|always|show|no|never|hide|maybe|defined]** - *sets the visibility of the stat*\n"
              <> "*Visibility 'maybe' and 'defined' hide the stat when the value is undefined.*\n"
              <> "**Stat names:** wins, losses, wlr, winsuntil, beststreak, currentstreak, bestdailystreak, bowhits, bowshots, accuracy\n"
              <> "**Example:** *?show accuracy* makes accuracy visible in the ?s command\n"
        pure ()
      "?show" -> commandTimeout 2 $ do
        let wrds = tail $ words $ unpack $ messageText m
        case wrds of
          [setting] -> setSetting dt sm m setting Nothing
          [setting, value] -> setSetting dt sm m setting (Just value)
          _ -> void $ restCall $ R.CreateMessage (messageChannel m) "*Wrong command syntax*"
      "?hide" -> commandTimeout 2 $ do
        let wrds = tail $ words $ unpack $ messageText m
        case wrds of
          [setting] -> setSetting dt sm m setting (Just "hide")
          _ -> void $ restCall $ R.CreateMessage (messageChannel m) "*Wrong command syntax*"
      _ -> pure ()
  _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isAdmin :: User -> Bool
isAdmin user = userId user == 422051538391793675