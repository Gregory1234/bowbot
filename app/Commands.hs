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
import Data.Maybe (fromMaybe, maybeToList, listToMaybe, isJust)
import Data.Char (toLower, isSpace)
import Control.Monad (when, void)
import Data.Text (unpack, strip, pack)
import Control.Concurrent (forkIO)
import System.Timeout (timeout)
import Control.Monad.Trans.Reader
import qualified Data.HashMap.Strict as HM
import Data.Aeson
import Stats
import Utils
import API
import Data.List (intercalate)

data BowBotData = BowBotData
  { hypixelRequestCount :: TVar Int,
    hypixelRequestBorderCount :: TVar Int,
    minecraftNicks :: TVar [(String, [String])],
    hypixelOnlineList :: TVar (Maybe [String]),
    hypixelOnlineBorderList :: TVar (Maybe [String]),
    hypixelOnlineBusyList :: TVar Bool,
    discordPeopleSettings :: TVar [(UserId, StatsSettings)],
    peopleSelectedAccounts :: TVar [(Integer, [UserId], String, [String])]
  }

minecraftNameToUUID' :: Manager -> TVar [(String, [String])] -> String -> IO (Maybe String)
minecraftNameToUUID' manager nameCache name = do
  cache <- atomically $ readTVar nameCache
  case filter ((name==) . head . snd) cache of
    [] -> minecraftNameToUUID manager name
    ((uuid, _) : _) -> return $ Just uuid

minecraftUuidToNames' :: Manager -> TVar [(String, [String])] -> String -> IO [String]
minecraftUuidToNames' manager nameCache uuid = do
  cache <- atomically $ readTVar nameCache
  case filter ((== uuid) . fst) cache of
    [] -> minecraftUuidToNames manager uuid
    ((_, names) : _) -> return names

flattenedMinecraftNicks :: BowBotData -> STM [(String, String)]
flattenedMinecraftNicks BowBotData {..} = do
  people <- readTVar minecraftNicks
  let currentNicks = [(n,u) | (n,us) <- people, u <- take 1 us]
  let restOfNicks = [(n,u) | (n,us) <- people, u <- drop 1 us]
  return $ currentNicks ++ restOfNicks


getHypixelStats' :: BowBotData -> Manager -> (String, String) -> IO (StatsResponse Stats)
getHypixelStats' bbd manager (uuid, name) = do
  stats <- if null uuid then return Nothing else liftIO $ getHypixelStats manager uuid
  people <- atomically $ flattenedMinecraftNicks bbd
  let decorate = \x c@Stats {..} -> c {playerName = if map toLower x == map toLower playerName then playerName else x ++ " (" ++ playerName ++ ")"}
  case stats of
    Nothing -> do
      let dists = map (\(u,n) -> ((u, n), dist (map toLower n) (map toLower name))) people
      let filtered = map fst . filter (\(_,d) -> d <= 2) $ dists
      case filtered of
        [] -> return NoResponse
        (nu:_) -> liftIO $ maybeToDidYouMeanStats (map toLower (snd nu) == map toLower name) . fmap (decorate (snd nu)) <$> getHypixelStats manager (fst nu)
    s@(Just Stats {bowWins = 0, bowLosses = 0}) -> do
      let dists = map (\(u,n) -> ((u, n), dist (map toLower n) (map toLower name))) people
      let filtered = map fst . filter (\(_,d) -> d <= 2) $ dists
      case filtered of
        [] -> return $ maybeToJustStats s
        (nu:_) -> liftIO $ maybeToDidYouMeanStats (map toLower (snd nu) == map toLower name) . fmap (decorate (snd nu)) <$> getHypixelStats manager (fst nu)
    s -> return $ maybeToJustStats s

statsCommand :: BowBotData -> Manager -> StatsSettings -> Message -> DiscordHandler ()
statsCommand dt@BowBotData {..} manager sett m = do
  t <- liftIO $ read @Int <$> getTime "%S"
  cv <- liftIO . atomically $ do
    c1 <- readTVar hypixelRequestCount
    c2 <- readTVar hypixelRequestBorderCount
    let c = c1 + c2
    when (c < 15) $ modifyTVar (if t <= 5 || t >= 55 then hypixelRequestBorderCount else hypixelRequestCount) (+ 1)
    return $ c < 15
  if cv
    then do
      let wrd = T.words (messageText m)
      (uuid, name) <- if length wrd == 1
      then do
        pns <- fmap (>>=(\(_, b, c, _) -> (,c) <$> b)) $ liftIO $ atomically $ readTVar peopleSelectedAccounts
        liftIO $ print $ lookup (userId $ messageAuthor m) pns
        let sel = lookup (userId $ messageAuthor m) pns
        return (sel, "")
      else do
        let name = ignoreChars "\\ " . unpack . strip . T.dropWhile isSpace . T.dropWhile (not . isSpace) $ messageText m
        fmap (, name) $ liftIO $ minecraftNameToUUID' manager minecraftNicks name
      if isJust uuid || not (null name) then do
        stats <- liftIO $ getHypixelStats' dt manager (fromMaybe "" uuid, name)
        _ <- case stats of
          NoResponse -> restCall $ R.CreateMessage (messageChannel m) "*The player doesn't exist!*"
          (JustResponse s) -> do
            when (bowWins s >= 500) $ do
              nicks <- liftIO $ atomically $ readTVar minecraftNicks
              case uuid of
                Nothing -> pure ()
                Just uid -> 
                  when (uid `notElem` map fst nicks) $ do
                    names <- liftIO $ minecraftUuidToNames manager uid
                    website <- liftIO $ fromMaybe "" <$> getEnv "DB_SITE"
                    apiKey <- liftIO $ fromMaybe "" <$> getEnv "DB_KEY"
                    let url = "http://" ++ website ++ "/addMinecraftName.php?key=" ++ apiKey ++ "&uuid=" ++ uid ++ "&names=" ++ intercalate "," names
                    _ <- liftIO $ sendRequestTo manager url
                    liftIO $ atomically $ writeTVar minecraftNicks $ (uid, names):nicks
            restCall . R.CreateMessage (messageChannel m) . pack . showStats sett $ s
          (DidYouMeanResponse s) -> restCall . R.CreateMessage (messageChannel m) . ("*Did you mean* "<>) . pack . showStats sett $ s
        pure ()
      else do
        _ <- restCall $ R.CreateMessage (messageChannel m) "*You aren't on the list! Please provide your ign to get added in the future.*"
        pure ()
    else do
      f <- liftIO $ read @Int <$> getTime "%S"
      _ <- restCall . R.CreateMessage (messageChannel m) . pack $ "**Too many requests! Wait another " ++ show ((65 - f) `mod` 60) ++ " seconds!**"
      pure ()

commandTimeout :: Int -> DiscordHandler () -> DiscordHandler ()
commandTimeout n x = ReaderT (void . forkIO . void . timeout (n * 1000000) . runReaderT x)

setSetting :: BowBotData -> Manager -> Message -> String -> Maybe String -> DiscordHandler ()
setSetting BowBotData {..} manager m setting maybeValue = case map toLower setting of
  "wins" -> setBool "wins" (\x y -> x {sWins = y})
  "losses" -> setBool "losses" (\x y -> x {sLosses = y})
  "wlr" -> setVal "wlr" (\x y -> x {sWLR = y})
  "winsuntil" -> setVal "winsUntil" (\x y -> x {sWinsUntil = y})
  "beststreak" -> setBool "bestStreak" (\x y -> x {sBestStreak = y})
  "currentstreak" -> setBool "currentStreak" (\x y -> x {sCurrentStreak = y})
  "bestdailystreak" -> setBool "bestDailyStreak" (\x y -> x {sBestDailyStreak = y})
  "bowhits" -> setBool "bowHits" (\x y -> x {sBowHits = y})
  "bowshots" -> setBool "bowShots" (\x y -> x {sBowShots = y})
  "accuracy" -> setVal "accuracy" (\x y -> x {sAccuracy = y})
  _ -> void $ restCall $ R.CreateMessage (messageChannel m) "*Wrong command argument!*"
  where
    val :: Maybe BoolSense
    val = case maybeValue of
      Nothing -> Just Always
      Just "yes" -> Just Always
      Just "show" -> Just Always
      Just "always" -> Just Always
      Just "no" -> Just Never
      Just "hide" -> Just Never
      Just "never" -> Just Never
      Just "maybe" -> Just WhenSensible
      Just "defined" -> Just WhenSensible
      _ -> Nothing
    bool :: Maybe Bool
    bool = case maybeValue of
      Nothing -> Just True
      Just "yes" -> Just True
      Just "show" -> Just True
      Just "always" -> Just True
      Just "no" -> Just False
      Just "hide" -> Just False
      Just "never" -> Just False
      _ -> Nothing
    fromBool :: Bool -> String
    fromBool False = "no"
    fromBool True = "yes"
    fromVal :: BoolSense -> String
    fromVal Never = "never"
    fromVal Always = "always"
    fromVal WhenSensible = "sensibly"
    sendReq :: UserId -> String -> String -> DiscordHandler ()
    sendReq did s v = do
      website <- liftIO $ fromMaybe "" <$> getEnv "DB_SITE"
      apiKey <- liftIO $ fromMaybe "" <$> getEnv "DB_KEY"
      let url = "http://" ++ website ++ "/updateSetting.php?key=" ++ apiKey ++ "&discord=" ++ show did ++ "&setting=" ++ s ++ "&value=" ++ v
      res <- liftIO $ sendRequestTo manager url
      _ <- case decode res :: Maybe Object of
          Nothing -> restCall $ R.CreateMessage (messageChannel m) "*Something went wrong!*"
          (Just js) -> do
            liftIO $ putStrLn $ "Received response from: " ++ url
            case js HM.!? "success" of
              (Just (Bool True)) -> restCall $ R.CreateMessage (messageChannel m) "*Successfully updated!*"
              _ -> restCall $ R.CreateMessage (messageChannel m) "*Something went wrong!*"
      pure ()
    setBool :: String -> (StatsSettings -> Bool -> StatsSettings) -> DiscordHandler ()
    setBool st upd = case bool of
      Nothing -> void $ restCall $ R.CreateMessage (messageChannel m) "*Wrong command argument!*"
      (Just b) -> do
        let did = userId $ messageAuthor m
        liftIO $ atomically $ do
          settings <- readTVar discordPeopleSettings
          writeTVar discordPeopleSettings $ map (\(i, s) -> (i, if i == did then upd s b else s)) (settings ++ [(did, upd defSettings b) | did `notElem` map fst settings])
        sendReq did st (fromBool b)
    setVal :: String -> (StatsSettings -> BoolSense -> StatsSettings) -> DiscordHandler ()
    setVal st upd = case val of
      Nothing -> void $ restCall $ R.CreateMessage (messageChannel m) "*Wrong command argument!*"
      (Just b) -> do
        let did = userId $ messageAuthor m
        liftIO $ atomically $ do
          settings <- readTVar discordPeopleSettings
          writeTVar discordPeopleSettings $ map (\(i, s) -> (i, if i == did then upd s b else s)) (settings ++ [(did, upd defSettings b) | did `notElem` map fst settings])
        sendReq did st (fromVal b)