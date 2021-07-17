{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Commands where

import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.Text as T
import Discord
import qualified Discord.Requests as R
import qualified Discord.Internal.Rest as R
import Discord.Types
import Network.HTTP.Conduit
import System.Environment.Blank (getEnv)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Char (toLower)
import Control.Monad (when, void, unless)
import Data.Text (unpack, pack)
import Control.Concurrent (forkIO)
import System.Timeout (timeout)
import Control.Monad.Trans.Reader
import qualified Data.HashMap.Strict as HM
import Data.Aeson
import Stats
import Utils
import API
import Data.List (intercalate)
import Data.Text.Encoding (encodeUtf8)

call :: (FromJSON a, R.Request (r a)) => r a -> DiscordHandler ()
call = void . restCall

respond :: Message -> String -> DiscordHandler ()
respond m = call . R.CreateMessage (messageChannel m) . pack

respondFile :: Message -> T.Text -> String -> DiscordHandler ()
respondFile m n = call . R.CreateMessageUploadFile (messageChannel m) n . encodeUtf8 . pack

sendRegisterMessage :: Message -> DiscordHandler ()
sendRegisterMessage m = respond m "*You aren't on the list! To register, type `?register yourign`.*"

data BowBotData = BowBotData
  { hypixelRequestCount :: TVar Int,
    hypixelRequestBorderCount :: TVar Int,
    minecraftNicks :: TVar [MinecraftAccount],
    hypixelOnlineList :: TVar (Maybe [String]),
    hypixelOnlineBorderList :: TVar (Maybe [String]),
    hypixelOnlineBusyList :: TVar Bool,
    discordPeopleSettings :: TVar [(UserId, StatsSettings)],
    peopleSelectedAccounts :: TVar [(Integer, [UserId], String, [String])],
    registeredNow :: TVar Int,
    leaderboardBusy :: TVar Bool
  }

minecraftNameToUUID' :: Manager -> TVar [MinecraftAccount] -> String -> IO (Maybe String)
minecraftNameToUUID' manager nameCache name = do
  cache <- atomically $ readTVar nameCache
  case filter ((map toLower name==) . map toLower . head . mcNames) cache of
    [] -> minecraftNameToUUID manager name
    (MinecraftAccount {..} : _) -> return $ Just mcUUID

minecraftUuidToNames' :: Manager -> TVar [MinecraftAccount] -> String -> IO [String]
minecraftUuidToNames' manager nameCache uuid = do
  cache <- atomically $ readTVar nameCache
  case filter ((== uuid) . mcUUID) cache of
    [] -> minecraftUuidToNames manager uuid
    (MinecraftAccount {..} : _) -> return mcNames

flattenedMinecraftNicks :: BowBotData -> STM [(String, String)]
flattenedMinecraftNicks BowBotData {..} = do
  people <- readTVar minecraftNicks
  let currentNicks = [(mcUUID,u) | MinecraftAccount {..} <- people, u <- take 1 mcNames]
  let restOfNicks = [(mcUUID,u) | MinecraftAccount {..} <- people, u <- drop 1 mcNames]
  return $ currentNicks ++ restOfNicks

registerCommand :: BowBotData -> Manager -> String -> Message -> DiscordHandler ()
registerCommand BowBotData {..} man name m = do
  uuid <- liftIO $ minecraftNameToUUID' man minecraftNicks name
  _ <- case uuid of
    Nothing -> respond m "*The player doesn't exist!*"
    Just uuid' -> do
      nicks <- liftIO $ atomically $ readTVar minecraftNicks
      taken <- fmap (>>=(\(_, _, _, b) -> b)) $ liftIO $ atomically $ readTVar peopleSelectedAccounts
      if uuid' `elem` taken
      then respond m "*That account already belongs to someone else!*"
      else do
        names <- liftIO $ minecraftUuidToNames' man minecraftNicks uuid'
        unless (uuid' `elem` map mcUUID nicks) $ do
          stats <- liftIO $ getHypixelStats man uuid'
          let hypixel = case stats of Nothing -> False; Just s -> bowWins s >= 500
          liftIO $ addMinecraftAccount man uuid' names hypixel
          liftIO $ atomically $ writeTVar minecraftNicks (MinecraftAccount {mcUUID = uuid', mcNames = names, mcHypixel = hypixel}:nicks)
        gid <- liftIO $ addAccount man (head names) (userId (messageAuthor m)) uuid'
        case gid of
          Nothing -> respond m "*Somehing went wrong*"
          Just gid' -> do
            psa <- liftIO $ atomically $ readTVar peopleSelectedAccounts
            liftIO $ atomically $ writeTVar peopleSelectedAccounts ((gid', [userId (messageAuthor m)], uuid', [uuid']):psa)
            respond m "*Registered successfully*"
  pure ()

withMinecraftFromName :: MonadIO m => Bool -> BowBotData -> Manager -> Maybe String -> UserId -> (String -> m (Maybe a)) -> m (StatsResponse a)
withMinecraftFromName _ BowBotData {..} manager Nothing author f = do
  pns <- fmap (>>=(\(_, b, c, _) -> (,c) <$> b)) $ liftIO $ atomically $ readTVar peopleSelectedAccounts
  liftIO $ print $ lookup author pns
  let uuid = lookup author pns
  case uuid of
    Nothing -> pure NotOnList
    Just uuid' -> do
      res <- f uuid'
      case res of
        Nothing -> pure NoResponse
        Just r -> do
          names <- liftIO $ minecraftUuidToNames' manager minecraftNicks uuid'
          pure (JustResponse (head names) r)
withMinecraftFromName b bbd@BowBotData {..} manager (Just (ignoreChars "\\ " -> name)) _ f = do
  if b
    then tryAutoCorrect
    else tryNormal
  where
    tryNormal = do
      uuid <- liftIO $ minecraftNameToUUID' manager minecraftNicks name
      case uuid of
        Nothing -> if b then pure NoResponse else tryAutoCorrect
        Just uuid' -> do
          res <- f uuid'
          case res of
            Nothing -> if b then pure NoResponse else tryAutoCorrect
            Just r -> do
              names <- liftIO $ minecraftUuidToNames' manager minecraftNicks uuid'
              pure (JustResponse (head names) r)
    tryAutoCorrect = do
      people <- liftIO $ atomically $ flattenedMinecraftNicks bbd
      let dists = map (\(u,n) -> ((u, n), dist (map toLower n) (map toLower name))) people
      let filtered = map fst . filter (\(_,d) -> d <= 2) $ dists
      case filtered of
        [] -> if b then tryNormal else pure NoResponse
        ((uuid, rn):_) -> do
          res <- f uuid
          case res of
            Nothing -> pure NoResponse
            Just r -> do
              names <- liftIO $ minecraftUuidToNames' manager minecraftNicks uuid
              if dist (map toLower $ head names) (map toLower name) <= 2
                then if map toLower name == map toLower rn
                  then pure (JustResponse rn r)
                  else pure (DidYouMeanResponse rn r)
                else if map toLower name == map toLower rn
                  then pure (OldResponse rn (head names) r)
                  else pure (DidYouMeanOldResponse rn (head names) r)

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
      let name = if length wrd == 1 then Nothing else Just $ unpack $ wrd !! 1
      stats <- liftIO $ withMinecraftFromName False dt manager name (userId $ messageAuthor m) $ \u -> do
        s <- getHypixelStats manager u
        case s of
          Nothing -> pure s
          (Just Stats {bowWins = 0, bowLosses = 0}) -> pure Nothing
          Just st -> do
            liftIO $ when (bowWins st >= 500) $ void $ forkIO $ do
              nicks <- atomically $ readTVar minecraftNicks
              when (u `notElem` map mcUUID nicks || all (\MinecraftAccount {..} -> mcUUID /= u || not mcHypixel) nicks) $ do
                names <- minecraftUuidToNames manager u
                website <- fromMaybe "" <$> getEnv "DB_SITE"
                apiKey <- fromMaybe "" <$> getEnv "DB_KEY"
                let url = "http://" ++ website ++ "/addMinecraftName.php?key=" ++ apiKey ++ "&uuid=" ++ u ++ "&hypixel=1&names=" ++ intercalate "," names
                _ <- sendRequestTo manager url
                atomically $ writeTVar minecraftNicks $ MinecraftAccount { mcUUID = u, mcNames = names, mcHypixel = True }:nicks
              nicks2 <- atomically $ readTVar minecraftNicks
              let acc = head $ filter (\MinecraftAccount {..} -> mcUUID == u) nicks2
              updateStats manager [(acc, Just st)]
            pure s
      _ <- case stats of
        NoResponse -> respond m "*The player doesn't exist!*"
        (JustResponse _ s) -> respond m . showStats sett $ s
        (OldResponse o _ s) -> respond m . showStats sett . addOldName o $ s
        (DidYouMeanResponse _ s) -> respond m . ("*Did you mean* " ++) . showStats sett $ s
        (DidYouMeanOldResponse o _ s) -> respond m . ("*Did you mean* " ++) . showStats sett . addOldName o $ s
        NotOnList -> sendRegisterMessage m
      pure ()
    else do
      f <- liftIO $ read @Int <$> getTime "%S"
      _ <- restCall . R.CreateMessage (messageChannel m) . pack $ "**Too many requests! Wait another " ++ show ((65 - f) `mod` 60) ++ " seconds!**"
      pure ()

urlCommand :: Bool -> BowBotData -> Manager -> (String -> String) -> Message -> DiscordHandler ()
urlCommand ac bbd man mkurl m = do
  let wrd = T.words (messageText m)
  url <- liftIO $ withMinecraftFromName ac bbd man (unpack <$> listToMaybe (tail wrd)) (userId $ messageAuthor m) $ \u -> do
    return (Just $ mkurl u)
  _ <- case url of
    NoResponse -> respond m "*The player doesn't exist!*"
    (JustResponse _ url') -> respond m url'
    (OldResponse _ _ url') -> respond m url'
    (DidYouMeanResponse n url') -> do
      respond m $ "*Did you mean* **" ++ n ++ "**:"
      respond m url'
    (DidYouMeanOldResponse n o url') -> do
      respond m $ "*Did you mean* **" ++ o ++ " (" ++ n ++ ")**:"
      respond m url'
    NotOnList -> sendRegisterMessage m
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
          Nothing -> respond m "*Something went wrong!*"
          (Just js) -> do
            liftIO $ putStrLn $ "Received response from: " ++ url
            case js HM.!? "success" of
              (Just (Bool True)) -> respond m "*Successfully updated!*"
              _ -> respond m "*Something went wrong!*"
      pure ()
    setBool :: String -> (StatsSettings -> Bool -> StatsSettings) -> DiscordHandler ()
    setBool st upd = case bool of
      Nothing -> respond m "*Wrong command argument!*"
      (Just b) -> do
        let did = userId $ messageAuthor m
        liftIO $ atomically $ do
          settings <- readTVar discordPeopleSettings
          writeTVar discordPeopleSettings $ map (\(i, s) -> (i, if i == did then upd s b else s)) (settings ++ [(did, upd defSettings b) | did `notElem` map fst settings])
        sendReq did st (fromBool b)
    setVal :: String -> (StatsSettings -> BoolSense -> StatsSettings) -> DiscordHandler ()
    setVal st upd = case val of
      Nothing -> respond m "*Wrong command argument!*"
      (Just b) -> do
        let did = userId $ messageAuthor m
        liftIO $ atomically $ do
          settings <- readTVar discordPeopleSettings
          writeTVar discordPeopleSettings $ map (\(i, s) -> (i, if i == did then upd s b else s)) (settings ++ [(did, upd defSettings b) | did `notElem` map fst settings])
        sendReq did st (fromVal b)