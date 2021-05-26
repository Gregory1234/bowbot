{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands where

import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Discord
import qualified Discord.Requests as R
import Discord.Types
import Network.HTTP.Conduit
import System.Environment.Blank (getEnv)
import Stats
import Utils
import API
import Data.Maybe (fromMaybe, maybeToList, listToMaybe, isNothing, isJust)
import Data.Char (toLower, isSpace)
import Control.Monad (when, unless, void)
import Data.Text (unpack, strip, pack)
import Control.Concurrent (forkIO)
import System.Timeout (timeout)
import Control.Monad.Trans.Reader

data BowBotData = BowBotData
  { count :: TVar Int,
    countBorder :: TVar Int,
    nickCache :: TVar [(String, [String])],
    online :: TVar (Maybe [String]),
    onlineBorder :: TVar (Maybe [String]),
    onlineBusy :: TVar Bool,
    peopleSettings :: TVar [(UserId, StatsSettings)],
    peopleNicks :: TVar [(UserId, String)]
  }

nameToUUID' :: Manager -> TVar [(String, [String])] -> String -> IO (Maybe String)
nameToUUID' manager nameCache name = do
  cache <- atomically $ readTVar nameCache
  case filter ((name==) . head . snd) cache of
    [] -> nameToUUID manager name
    ((uuid, _) : _) -> return $ Just uuid

uuidToNames' :: Manager -> TVar [(String, [String])] -> String -> IO [String]
uuidToNames' manager nameCache uuid = do
  cache <- atomically $ readTVar nameCache
  case filter ((== uuid) . fst) cache of
    [] -> uuidToNames manager uuid
    ((_, names) : _) -> return names

addNick :: Manager -> TVar [(UserId, String)] -> UserId -> String -> IO ()
addNick manager peopleNicks did uuid = do
  atomically $ do
    n <- readTVar peopleNicks
    writeTVar peopleNicks $ (did, uuid):filter ((/=did).fst) n
  website <- fromMaybe "" <$> getEnv "DB_SITE"
  apiKey <- fromMaybe "" <$> getEnv "DB_KEY"
  let url = "http://" ++ website ++ "/addPerson.php?key=" ++ apiKey ++ "&discord=" ++ show did ++ "&minecraft=" ++ uuid
  _ <- sendRequestTo manager url
  pure ()

flatNickList :: BowBotData -> IO [(String, String)]
flatNickList BowBotData {..} = do
  people <- atomically $ readTVar nickCache
  let currentNicks = [(n,u) | (n,us) <- people, u <- maybeToList (listToMaybe us)]
  let restOfNicks = [(n,u) | (n,us) <- people, u <- drop 1 us]
  return $ currentNicks ++ restOfNicks


searchForStats :: BowBotData -> Manager -> (String, String) -> IO (StatsResponse Stats)
searchForStats bbd manager (uuid, name) = do
  stats <- if null uuid then return Nothing else liftIO $ getStats manager uuid
  people <- flatNickList bbd
  case stats of
    Nothing -> do
      let dists = map (\(u,n) -> (u, dist (map toLower n) (map toLower name))) people
      let filtered = map fst . filter (\(_,d) -> d <= 2) $ dists
      case filtered of
        [] -> return NoResponse
        (nu:_) -> liftIO $ maybeToDidYouMeanStats <$> getStats manager nu
    s@(Just Stats {bowWins = 0, bowLosses = 0}) -> do
      let dists = map (\(u,n) -> (u, dist (map toLower n) (map toLower name))) people
      let filtered = map fst . filter (\(_,d) -> d <= 2) $ dists
      case filtered of
        [] -> return $ maybeToJustStats s
        (nu:_) -> liftIO $ maybeToDidYouMeanStats <$> getStats manager nu
    s -> return $ maybeToJustStats s

statsCommand :: BowBotData -> Manager -> StatsSettings -> Message -> DiscordHandler ()
statsCommand dt@BowBotData {..} manager sett m = do
  t <- liftIO $ read @Int <$> getTime "%S"
  cv <- liftIO . atomically $ do
    c1 <- readTVar count
    c2 <- readTVar countBorder
    let c = c1 + c2
    when (c < 15) $ modifyTVar (if t <= 5 || t >= 55 then countBorder else count) (+ 1)
    return $ c < 15
  if cv
    then do
      let wrd = T.words (messageText m)
      (uuid, name) <- if length wrd == 1
      then do
        pns <- liftIO $ atomically $ readTVar peopleNicks
        liftIO $ print $ lookup (userId $ messageAuthor m) pns
        return (lookup (userId $ messageAuthor m) pns, "")
      else do
        let name = unpack . strip . T.dropWhile isSpace . T.dropWhile (not . isSpace) $ messageText m
        fmap (, name) $ liftIO $ nameToUUID' manager nickCache name
      if isJust uuid then do
        stats <- liftIO $ searchForStats dt manager (fromMaybe "" uuid, name)
        _ <- case stats of
          NoResponse -> restCall $ R.CreateMessage (messageChannel m) "*The player doesn't exist!*"
          (JustResponse s) -> restCall . R.CreateMessage (messageChannel m) . pack . showStats sett $ s
          (DidYouMeanResponse s) -> restCall . R.CreateMessage (messageChannel m) . ("*Did you mean* "<>) . pack . showStats sett $ s
        pure ()
      else do
        _ <- restCall $ R.CreateMessage (messageChannel m) $ if null name then "*You aren't on the list! Please provide your ign to get added in the future.*" else "*The player doesn't exist!*"
        pure ()
    else do
      f <- liftIO $ read @Int <$> getTime "%S"
      _ <- restCall . R.CreateMessage (messageChannel m) . pack $ "**Too many requests! Wait another " ++ show ((65 - f) `mod` 60) ++ " seconds!**"
      pure ()

commandTimeout :: Int -> DiscordHandler () -> DiscordHandler ()
commandTimeout n x = ReaderT (void . forkIO . void . timeout (n * 1000000) . runReaderT x)