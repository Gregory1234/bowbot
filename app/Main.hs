{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Applicative ((<|>))
import Control.Concurrent (forkFinally, forkIO, threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM
import Control.Monad (forever, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe, catMaybes, isJust)
import Data.Text (pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Discord
import qualified Discord.Requests as R
import Discord.Types
import Network.HTTP.Conduit
import System.Environment.Blank (getEnv)
import Stats
import Utils
import API
import Commands
import Data.List.Split (chunksOf)
import Data.Traversable (for)
import Data.Text.Encoding (encodeUtf8)
import Control.Monad.Reader (ReaderT(..))
import Data.Either (fromRight)
import Data.List (find, sortOn)
import Text.Read (readMaybe)
import Data.Monoid (First(..))



main :: IO ()
main = do
  apiKey <- fromMaybe "" <$> getEnv "API_KEY"
  unless (apiKey == "") $ do
    hypixelRequestCount <- atomically $ newTVar 0
    hypixelRequestBorderCount <- atomically $ newTVar 0
    minecraftNicks <- atomically $ newTVar []
    hypixelOnlineList <- atomically $ newTVar Nothing
    hypixelOnlineBorderList <- atomically $ newTVar Nothing
    hypixelOnlineBusyList <- atomically $ newTVar False
    discordPeopleSettings <- atomically $ newTVar []
    peopleSelectedAccounts <- atomically $ newTVar []
    registeredNow <- atomically $ newTVar 0
    leaderboardBusy <- atomically $ newTVar False
    let bbdata = BowBotData {..}
    downloadData bbdata
    mkBackground bbdata
    manager <- newManager managerSettings
    forever $ do
      userFacingError <-
        runDiscord $
          def
            { discordToken = pack apiKey,
              discordOnStart = onStartup bbdata,
              discordOnEvent = eventHandler bbdata manager
            }
      TIO.putStrLn userFacingError

onStartup :: BowBotData -> DiscordHandler ()
onStartup bbdata = do
  sendCommand (UpdateStatus $ UpdateStatusOpts {
    updateStatusOptsSince = Nothing, 
    updateStatusOptsGame = Just (Activity {activityName = "try out ?settings command", activityType = ActivityTypeGame, activityUrl = Nothing}),
    updateStatusOptsNewStatus = UpdateStatusOnline,
    updateStatusOptsAFK = False
  })
  mkBackgroundDiscord bbdata

downloadData :: BowBotData -> IO ()
downloadData BowBotData {..} = do
  manager <- newManager managerSettings
  _ <- forkIO $ downloadNicks manager minecraftNicks
  _ <- forkIO $ updateSettings manager discordPeopleSettings peopleSelectedAccounts
  pure ()

updateData :: BowBotData -> IO ()
updateData BowBotData {..} = do
  manager <- newManager managerSettings
  _ <- forkIO $ updateNicks manager minecraftNicks
  _ <- forkIO $ updateSettings manager discordPeopleSettings peopleSelectedAccounts
  pure ()

updateLeaderboard :: BowBotData -> IO ()
updateLeaderboard BowBotData {..} = void . forkIO $ do
  lb <- atomically $ do
    a <- readTVar leaderboardBusy
    b <- readTVar hypixelOnlineBusyList
    c <- readTVar hypixelOnlineList
    d <- readTVar hypixelOnlineBorderList
    return (a || b || isJust c || isJust d)
  unless lb $ do
    manager <- newManager managerSettings
    nickList <- atomically $ readTVar minecraftNicks
    let chunked = chunksOf 50 (filter mcHypixel nickList)
    putStrLn "Started updating leaderboards"
    atomically $ writeTVar leaderboardBusy True
    void $ for chunked (helper manager)
    atomically $ writeTVar leaderboardBusy False
    putStrLn "Stopped updating leaderboards"
  where
    helper :: Manager -> [MinecraftAccount] -> IO ()
    helper manager lst = do
      let chunked = chunksOf 10 lst
      dt <- fmap (zip lst . concat) $ for chunked $ mapConcurrently (getHypixelStats manager . mcUUID)
      updateStats manager dt
      threadDelay 65000000

downloadNicks :: Manager -> TVar [MinecraftAccount] -> IO ()
downloadNicks manager nickCache = do
  nickList <- getMinecraftNickList manager
  atomically $ writeTVar nickCache nickList

updateNicks :: Manager -> TVar [MinecraftAccount] -> IO ()
updateNicks manager nickCache = do
  nickList <- getMinecraftNickList manager
  let chunked = chunksOf 10 nickList
  updatedNicks <- fmap concat $ for chunked $ mapConcurrently helper
  atomically $ writeTVar nickCache updatedNicks
  where
    helper MinecraftAccount {..} = do
      newNames <- minecraftUuidToNames manager mcUUID
      unless (mcNames == newNames) $ do
        updateMinecraftNames manager mcUUID newNames
      return MinecraftAccount {mcNames = newNames, ..}

updateSettings :: Manager -> TVar [(UserId, StatsSettings)] -> TVar [(Integer, [UserId], String, [String])] -> IO ()
updateSettings manager peopleSettings peopleNicks = do
  settings <- getPeopleSettings manager
  nicks <- getPeopleSelectedAccounts manager
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
          bc <- readTVar hypixelRequestBorderCount
          writeTVar hypixelRequestCount bc
          writeTVar hypixelRequestBorderCount 0
          writeTVar registeredNow 0
        atomically $ do
          onl <- readTVar hypixelOnlineBorderList
          writeTVar hypixelOnlineList onl
          writeTVar hypixelOnlineBorderList Nothing
        when (mint == "00") $ updateData bbdata
        when (mint == "30") $ updateLeaderboard bbdata
        putStrLn "New minute finished!"
      threadDelay 60000000

mkBackgroundDiscord :: BowBotData -> DiscordHandler ()
mkBackgroundDiscord bbdata = ReaderT $ \x -> void $
  forkFinally (runReaderT (backgroundDiscord bbdata) x) $ \e -> do
    print e
    runReaderT (mkBackgroundDiscord bbdata) x

backgroundDiscord :: BowBotData -> DiscordHandler ()
backgroundDiscord bbdata = do
  sec <- liftIO $ read @Int <$> getTime "%S"
  liftIO $ threadDelay ((60 - sec `mod` 60) * 1000000)
  forever go
  where
    go = do
      _ <- ReaderT $ \x -> forkIO $ flip runReaderT x $ do
        mint <- liftIO $ getTime "%M"
        when (mint == "00") do
          updateDiscords'
          updateRoles' bbdata
      liftIO $ threadDelay 60000000

airplanesId :: GuildId
airplanesId = 742731987902791751

roleIdToTitle :: RoleId -> Maybe DivisionTitle
roleIdToTitle 742734559514329239 = Just MasterTitle
roleIdToTitle 742734346200285277 = Just LegendTitle
roleIdToTitle 742734209084555264 = Just GrandmasterTitle
roleIdToTitle 742734125231898656 = Just GodlikeTitle
roleIdToTitle 770446876411035669 = Just GodlikeXTitle
roleIdToTitle _ = Nothing

titleRoleId :: DivisionTitle -> Maybe RoleId
titleRoleId NoDivisionTitle = Nothing
titleRoleId MasterTitle = Just 742734559514329239
titleRoleId LegendTitle = Just 742734346200285277
titleRoleId GrandmasterTitle = Just 742734209084555264
titleRoleId GodlikeTitle = Just 742734125231898656
titleRoleId GodlikeXTitle = Just 770446876411035669

updateDiscords' :: DiscordHandler ()
updateDiscords' = do
  manager <- liftIO $ newManager managerSettings
  uids <- liftIO $ getDiscordIds manager
  v <- fmap (filter (not . userIsBot . memberUser)) <$> restCall (R.ListGuildMembers airplanesId R.GuildMembersTiming {R.guildMembersTimingLimit = Just 500, R.guildMembersTimingAfter = Nothing})
  case v of
    Right x -> do
      let uids' = filter (\u -> all (\m -> userId (memberUser m) /= u) x) uids
      y <- traverse helper uids'
      liftIO $ updateDiscords manager x y
    Left x -> liftIO $ print x
  where
    helper :: UserId -> DiscordHandler User
    helper u = do
      y <- restCall (R.GetUser u)
      return $ fromRight undefined y

updateRoles' :: BowBotData -> DiscordHandler ()
updateRoles' bbd = do
  manager <- liftIO $ newManager managerSettings
  uids <- liftIO $ getDiscordRoleDisabledIds manager
  v <- fmap (filter (not . userIsBot . memberUser)) <$> restCall (R.ListGuildMembers airplanesId R.GuildMembersTiming {R.guildMembersTimingLimit = Just 500, R.guildMembersTimingAfter = Nothing})
  case v of
    Right x -> do
      let members = filter (\m -> userId (memberUser m) `notElem` uids) x
      lb <- liftIO $ getMinecraftStatList manager
      _ <- traverse (updateRoles bbd lb) members
      pure ()
    Left x -> liftIO $ print x

updateRoles :: BowBotData -> [(String, Int, Int, Int)] -> GuildMember -> DiscordHandler Bool
updateRoles BowBotData {..} lb memb = do
  pns <- fmap (>>=(\(_, b, _, d) -> (,d) <$> b)) $ liftIO $ atomically $ readTVar peopleSelectedAccounts
  let did = userId . memberUser $ memb
  case lookup did pns of
    Nothing -> pure False
    Just uuids -> do
      let lb' = map (\(a, b, _, _) -> (a,b)) lb
      let wins' = mapMaybe (`lookup` lb') uuids
      unless (null wins') $ do
        let wins = maximum wins'
        let title = winsToTitle (fromIntegral wins)
        let currentRoles = memberRoles memb
        let currentTitle = getFirst $ mconcat (map (First . roleIdToTitle) currentRoles)
        case (currentTitle >>= titleRoleId, titleRoleId title) of
          (Nothing, Nothing) -> pure ()
          (Nothing, Just rid) -> call $ R.AddGuildMemberRole airplanesId did rid
          (Just rid, Nothing) -> call $ R.RemoveGuildMemberRole airplanesId did rid
          (Just rid, Just rid') -> when (rid /= rid') $ do
            call $ R.RemoveGuildMemberRole airplanesId did rid
            call $ R.AddGuildMemberRole airplanesId did rid'
      pure (not $ null wins')

eventHandler :: BowBotData -> Manager -> Event -> DiscordHandler ()
eventHandler dt@BowBotData {..} sm event = case event of
  MessageCreate m -> do
    unless (fromBot m) $ case unpack $ T.toLower . T.takeWhile (/= ' ') $ messageText m of
      "?s" -> commandTimeout 12 $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        settings <- liftIO $ atomically $ readTVar discordPeopleSettings
        statsCommand dt sm (fromMaybe defSettings $ lookup (userId $ messageAuthor m) settings) m
      "?sd" -> commandTimeout 12 $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        statsCommand dt sm defSettings m
      "?sa" -> commandTimeout 12 $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        statsCommand dt sm allSettings m
      "?register" -> commandTimeout 12 $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        rn <- liftIO $ atomically $ readTVar registeredNow
        if rn > 2
        then respond m "*Too many requests! Please wait!*"
        else do
          liftIO $ atomically $ writeTVar registeredNow (rn + 1)
          let wrd = T.words (messageText m)
          pns <- fmap (>>=(\(_, b, _, _) -> b)) $ liftIO $ atomically $ readTVar peopleSelectedAccounts
          if userId (messageAuthor m) `elem` pns
          then respond m "*You are already registered. If you made a mistake, contact me (**GregC**#9698)*"
          else if length wrd /= 2
            then respond m "*Wrong command syntax*"
            else registerCommand dt sm (unpack $ wrd !! 1) m
      "?na" -> commandTimeout 12 $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        let wrd = T.words (messageText m)
        names <- liftIO $ withMinecraftFromName False dt sm (unpack <$> listToMaybe (tail wrd)) (userId $ messageAuthor m) $ \u -> do
          names <- minecraftUuidToNames' sm minecraftNicks u
          return if null names then Nothing else Just names
        case names of
          NoResponse -> respond m "*The player doesn't exist!*"
          (JustResponse _ s) -> respond m $ "```\n" ++ unlines (reverse s) ++ "```"
          (OldResponse _ _ s) -> respond m $ "```\n" ++ unlines (reverse s) ++ "```"
          (DidYouMeanResponse n s) -> respond m $ "*Did you mean* **" ++ n ++ "**:```\n" ++ unlines (reverse s) ++ "```"
          (DidYouMeanOldResponse n o s) -> respond m $ "*Did you mean* **" ++ o ++ " (" ++ n ++ ")**:```\n" ++ unlines (reverse s) ++ "```"
          NotOnList -> sendRegisterMessage m
      "?n" -> commandTimeout 12 $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        let wrd = T.words (messageText m)
        names <- liftIO $ withMinecraftFromName True dt sm (unpack <$> listToMaybe (tail wrd)) (userId $ messageAuthor m) $ \u -> do
          names <- minecraftUuidToNames' sm minecraftNicks u
          return if null names then Nothing else Just names
        _ <- case names of
          NoResponse -> respond m "*The player doesn't exist!*"
          (JustResponse _ s) -> respond m $ "```\n" ++ unlines (reverse s) ++ "```"
          (OldResponse _ _ s) -> respond m $ "```\n" ++ unlines (reverse s) ++ "```"
          (DidYouMeanResponse n s) -> respond m $ "*Did you mean* **" ++ n ++ "**:```\n" ++ unlines (reverse s) ++ "```"
          (DidYouMeanOldResponse n o s) -> respond m $ "*Did you mean* **" ++ o ++ " (" ++ n ++ ")**:```\n" ++ unlines (reverse s) ++ "```"
          NotOnList -> sendRegisterMessage m
        pure ()
      "?head" -> commandTimeout 12 $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        urlCommand True dt sm (\s -> "https://crafatar.com/avatars/" ++ s ++ "?overlay") m
      "?skin" -> commandTimeout 12 $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        urlCommand True dt sm (\s -> "https://crafatar.com/renders/body/" ++ s ++ "?overlay") m
      "?heada" -> commandTimeout 12 $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        urlCommand False dt sm (\s -> "https://crafatar.com/avatars/" ++ s ++ "?overlay") m
      "?skina" -> commandTimeout 12 $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        urlCommand False dt sm (\s -> "https://crafatar.com/renders/body/" ++ s ++ "?overlay") m
      "?online" -> commandTimeout 20 $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        t <- liftIO $ read @Int <$> getTime "%S"
        cv <- liftIO . atomically $ do
          c <- readTVar hypixelOnlineBusyList
          lb <- readTVar leaderboardBusy
          unless (c || lb) $ writeTVar hypixelOnlineBusyList True
          return (not c && not lb)
        if cv
          then do
            onlo <- liftIO . atomically $ readTVar hypixelOnlineList
            onlb <- liftIO . atomically $ readTVar hypixelOnlineList
            manager <- liftIO $ newManager managerSettings
            o <- case onlo <|> onlb of
              (Just onl) -> do
                respond m "**Players in wachList currently in bow duels:** (cached response)"
                pure onl
              Nothing -> do
                people <- liftIO $ getWatchlist manager
                status <- liftIO $ mapConcurrently (\u -> (u,) . fromMaybe False <$> isInBowDuels manager u) people
                let onl = map fst $ filter snd status
                liftIO . atomically $ writeTVar (if t <= 5 || t >= 55 then hypixelOnlineBorderList else hypixelOnlineList) $ Just onl
                respond m "**Players in wachList currently in bow duels:**"
                pure onl
            liftIO . atomically $ writeTVar hypixelOnlineBusyList False
            names <- liftIO $ traverse (fmap head . minecraftUuidToNames' manager minecraftNicks) o
            let msg = if null names then "None of the watchListed players are currently in bow duels." else unlines . map (" - " ++) $ names
            respond m $ "```" ++ msg ++ "```"
          else respond m "**Processing list of online players. Please send command again later.**"
      "?lb" -> commandTimeout 10 $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        dat <- liftIO $ getMinecraftStatList sm
        lb <- traverse (\(u,a,_,_) -> do
          n <- liftIO $ minecraftUuidToNames' sm minecraftNicks u
          pure (head n, a)) dat
        let leaderboard = zip [1..] $ sortOn (negate . snd) lb
        let leaderboardString = map (\(i,(n, v)) -> pad 5 (show i ++ ".") ++ pad 20 n ++ " ( " ++ show v ++ " Wins )") leaderboard
        let pages = map unlines $ chunksOf 20 leaderboardString
        let wrds = tail $ words $ unpack $ messageText m
        case wrds of
          ["all"] -> void $ restCall $ R.CreateMessageUploadFile (messageChannel m) "list.txt" . encodeUtf8 . pack $ unlines leaderboardString
          [(readMaybe @Int -> Just n)] -> if n > 0 && n <= length pages
            then respond m $ "Hypixel Bow Duels Leaderboard (page " ++ show n ++ "):```\n"  ++ pages !! (n-1) ++ "```"
            else respond m "*Wrong page number*"
          [] -> respond m $ "Hypixel Bow Duels Leaderboard (page 1):```\n"  ++ head pages ++ "```"
          _ -> respond m "*Wrong command syntax*"
      "?list" -> commandTimeout 2 $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        let wrds = tail $ words $ unpack $ messageText m
        ln <- case wrds of
          [] -> do
            manager <- liftIO $ newManager managerSettings
            st <- liftIO $ getWatchlist manager
            people <- traverse (liftIO . minecraftUuidToNames' manager minecraftNicks) st
            return $ Just (mapMaybe listToMaybe $ people, "Players in watchList")
          ["w"] -> do
            manager <- liftIO $ newManager managerSettings
            st <- liftIO $ getWatchlist manager
            people <- traverse (liftIO . minecraftUuidToNames' manager minecraftNicks) st
            return $ Just (mapMaybe listToMaybe $ people, "Players in watchList")
          ["ac"] -> do
            st <- liftIO $ atomically $ readTVar minecraftNicks
            return $ Just (mapMaybe (listToMaybe . mcNames) st, "Players on autocomplete list")
          ["d"] -> do
            manager <- liftIO $ newManager managerSettings
            st <- liftIO $ atomically $ readTVar peopleSelectedAccounts
            people <- traverse (liftIO . minecraftUuidToNames' manager minecraftNicks . (\(_,_,x, _) -> x)) st
            return $ Just (mapMaybe listToMaybe $ people, "Players on discord list")
          _ -> return Nothing
        case ln of
          Nothing -> respond m "*Wrong command syntax*"
          Just (list, name) ->
            if sum (map length list) < 1800
              then respond m $ "**" ++ name ++ ":**\n" ++ "```\n" ++ unwords list ++ "```"
              else do
                _ <- respond m $ "**" ++ name ++ ":**"
                respondFile m "list.txt" $ unlines list
      "?help" -> commandTimeout 2 $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        respond m $"**Bow bot help:**\n\n"
          ++ "**Commands:**\n"
          ++ " - **?help** - *display this message*\n"
          ++ " - **?online** - *show all people from watchList currently in Bow Duels*\n"
          ++ " - **?list** - *show all players in watchList*\n"
          ++ " - **?s [name]** - *show player's Bow Duels stats*\n"
          ++ " - **?sa [name]** - *show all Bow Duels stats*\n"
          ++ " - **?sd [name]** - *show a default set of Bow Duels stats*\n"
          ++ " - **?n(a) [name]** - *show player's past nicks*\n"
          ++ " - **?head(a) [name]** - *show player's head*\n"
          ++ " - **?skin(a) [name]** - *show player's full skin*\n"
          ++ " - **?lb [page]** - *show a Bow Duels Wins leaderboard*\n"
          ++ " - **?lb all** - *upload a file with the whole Bow Duels Wins leaderboard*\n"
          ++ " - **?mc** - *list your linked minecraft nicks*\n"
          ++ " - **?mc [name]** - *select a minecraft account as your default*\n"
          ++ " - **?roles** - *refresh discord roles*\n"
          ++ " - **?settings** - *display help for settings*\n"
          ++ "\nMade by **GregC**#9698"
      "?refresh" -> commandTimeout 2 $ when (isAdmin (messageAuthor m)) $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        liftIO $ downloadData dt
      "?fullrefresh" -> commandTimeout 200 $ when (isAdmin (messageAuthor m)) $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        liftIO $ updateData dt
        updateDiscords'
      "?discordrefresh" -> commandTimeout 200 $ when (isAdmin (messageAuthor m)) $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        updateDiscords'
      "?rolesrefresh" -> commandTimeout 200 $ when (isAdmin (messageAuthor m)) $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        updateRoles' dt
      "?lbrefresh" -> commandTimeout 200 $ when (isAdmin (messageAuthor m)) $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        liftIO $ updateLeaderboard dt
      "?mc" -> commandTimeout 2 $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        let wrds = tail $ words $ unpack $ messageText m
        st <- liftIO $ atomically $ readTVar peopleSelectedAccounts
        let accountMaybe = find (\(_,b,_,_) -> userId (messageAuthor m) `elem` b) st
        case wrds of
          [] -> case accountMaybe of
            Nothing -> sendRegisterMessage m
            Just (_, _, sel, mc) -> do
              let helper = \x -> do {
                  name <- liftIO $ head <$> minecraftUuidToNames' sm minecraftNicks x;
                  return $ (if sel == x then "*" else "") ++ name
                }
              mc' <- traverse helper mc
              respond m $ "**List of your minecraft nicks linked:**\n```\n" ++ unlines mc' ++ "```"
          [newsel] -> case accountMaybe of
            Nothing -> sendRegisterMessage m
            Just (gid, dids, _, mc) -> do
              newselid <- liftIO $ minecraftNameToUUID' sm minecraftNicks newsel
              case newselid of
                Nothing -> respond m "*Player doesn't exist!*"
                Just nid -> if nid `elem` mc
                  then do
                    website <- liftIO $ fromMaybe "" <$> getEnv "DB_SITE"
                    apiKey <- liftIO $ fromMaybe "" <$> getEnv "DB_KEY"
                    let url = "http://" ++ website ++ "/selectMinecraft.php?key=" ++ apiKey ++ "&id=" ++ show gid ++ "&minecraft=" ++ nid
                    _ <- liftIO $ sendRequestTo sm url
                    liftIO $ atomically $ writeTVar peopleSelectedAccounts $ map (\u@(i, _, _, _) -> if i == gid then (gid, dids, nid, mc) else u) st
                    respond m "*Success!*"
                  else respond m "*You do not have that minecraft nick linked!*"
          _ -> respond m "*Wrong command syntax*"
        pure ()
      "?roles" -> commandTimeout 12 $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        lb <- liftIO $ getMinecraftStatList sm
        mem <- restCall $ R.GetGuildMember airplanesId (userId $ messageAuthor m)
        case mem of
          Left _ -> pure ()
          Right mem' -> do
            liftIO $ updateDiscords sm [mem'] []
            b <- updateRoles dt lb mem'
            if b
            then respond m "Roles updated successfully."
            else respond m "Something went wrong!"
        pure ()
      "?settings" -> commandTimeout 2 $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        respond m $ "**You can now customize the output of ?s command!**\n"
          ++ "**Commands:**\n"
          ++ " - **?settings** - *display this message*\n"
          ++ " - **?show [stat]** - *makes the stat visible*\n"
          ++ " - **?hide [stat]** - *makes the stat hidden*\n"
          ++ " - **?show [stat] [yes|always|show|no|never|hide|maybe|defined]** - *sets the visibility of the stat*\n"
          ++ "*Visibility 'maybe' and 'defined' hide the stat when the value is undefined.*\n"
          ++ "**Stat names:** wins, losses, wlr, winsuntil, beststreak, currentstreak, bestdailystreak, bowhits, bowshots, accuracy\n"
          ++ "**Example:** *?show accuracy* makes accuracy visible in the ?s command\n"
        pure ()
      "?show" -> commandTimeout 2 $ do
        let wrds = tail $ words $ unpack $ messageText m
        case wrds of
          [setting] -> setSetting dt sm m setting Nothing
          [setting, value] -> setSetting dt sm m setting (Just value)
          _ -> respond m "*Wrong command syntax*"
      "?hide" -> commandTimeout 2 $ do
        let wrds = tail $ words $ unpack $ messageText m
        case wrds of
          [setting] -> setSetting dt sm m setting (Just "hide")
          _ -> respond m "*Wrong command syntax*"
      _ -> pure ()
  _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isAdmin :: User -> Bool
isAdmin user = userId user == 422051538391793675