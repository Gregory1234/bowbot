{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module BowBot.Background where

import Discord
import qualified Discord.Requests as R
import BowBot.BotData
import BowBot.Stats.HypixelBow
import BowBot.API
import BowBot.DB
import Network.HTTP.Conduit (newManager, httpLbs, parseRequest)
import Data.List.Split (chunksOf)
import Control.Concurrent (threadDelay)
import Data.Map ((!?))
import Control.Concurrent.Async (mapConcurrently)
import Data.Maybe (catMaybes, mapMaybe)
import Discord.Types hiding (accountId)
import Data.Aeson.Types (object, (.=))
import Data.Either (fromRight)
import Data.List ((\\), intercalate)
import BowBot.Command
import BowBot.Birthday
import Control.Exception.Base (SomeException, try)
import Control.Monad.Trans (lift)

announceBirthdays :: BotData -> ManagerT DiscordHandler ()
announceBirthdays bdt = do
  currentDay <- liftIO currentBirthdayDate
  maybeBirthdays <- getBirthdayPeople currentDay
  case maybeBirthdays of
    Nothing -> hLogError "Birthday parsing failed!"
    Just birthdays -> do
      dgid <- readProp discordGuildId bdt
      people <- lift $ fmap (filter ((`elem` birthdays) . userId . memberUser)) <$> call (R.ListGuildMembers dgid R.GuildMembersTiming {R.guildMembersTimingLimit = Just 500, R.guildMembersTimingAfter = Nothing})
      case people of
        Left err -> hLogError $ show err
        Right ppl -> unless (null ppl) $ do
          birthdayChannel <- readProp discordBirthdayChannel bdt
          hLogInfo $ "Announcing birthdays: " ++ intercalate ", " (map (showMemberOrUser True . Right) ppl)
          lift $ for_ ppl $ \p -> call $ R.CreateMessage birthdayChannel $ pack $ "**Happy birthday** to " ++ showMemberOrUser True (Right p) ++ "!"

updateDiscordStatus :: DiscordHandler ()
updateDiscordStatus = do
  status <- withDB $ flip getInfoDB "discord_status"
  sendCommand (UpdateStatus $ UpdateStatusOpts {
      updateStatusOptsSince = Nothing,
      updateStatusOptsGame = case status of
         Nothing -> Nothing
         Just s -> Just (Activity {activityName = pack s, activityType = ActivityTypeGame, activityUrl = Nothing}),
      updateStatusOptsNewStatus = UpdateStatusOnline,
      updateStatusOptsAFK = False
    })

updateDivisionRolesSingle :: BotData -> Map String HypixelBowLeaderboards -> GuildMember -> BowBotAccount -> DiscordHandler ()
updateDivisionRolesSingle bdt lb memb BowBotAccount { accountMinecrafts = mc } = do
  gid <- readProp discordGuildId bdt
  let did = userId . memberUser $ memb
  let wins = maximum $ mapMaybe (fmap bowLbWins . (lb !?)) mc
  divisionRoles <- readProp discordDivisionRoles bdt
  let currentDivisionRoles = filter (`elem` map snd divisionRoles) (memberRoles memb)
  let targetDivisionRoles = take 1 $ map snd $ filter ((<= wins) . fst) $ reverse divisionRoles
  for_ (currentDivisionRoles \\ targetDivisionRoles) $ call . R.RemoveGuildMemberRole gid did
  for_ (targetDivisionRoles \\ currentDivisionRoles) $ call . R.AddGuildMemberRole gid did

updateGuildMemberRolesSingle :: BotData -> [String] -> GuildMember -> BowBotAccount -> DiscordHandler ()
updateGuildMemberRolesSingle bdt members memb BowBotAccount { accountMinecrafts = mc } = do
  gid <- readProp discordGuildId bdt
  let did = userId . memberUser $ memb
  let isMember = any (`elem` members) mc
  memberRole <- readProp discordMemberRole bdt
  visitorRole <- readProp discordVisitorRole bdt
  let currentRoles = filter (\x -> x == memberRole || x == visitorRole) (memberRoles memb)
  let targetRoles = [if isMember then memberRole else visitorRole]
  for_ (currentRoles \\ targetRoles) $ call . R.RemoveGuildMemberRole gid did -- TODO: remove repetition
  for_ (targetRoles \\ currentRoles) $ call . R.AddGuildMemberRole gid did

-- TODO: remove BotData from here - take it out to a new type
updateDiscordRolesSingle
  :: BotData -> Maybe (Map String HypixelBowLeaderboards) -> Maybe [String] -> GuildId
  -> GuildMember -> Maybe BowBotAccount -> DiscordHandler ()
updateDiscordRolesSingle bdt lb members gid m (Just bac) = do
  for_ lb $ \x -> updateDivisionRolesSingle bdt x m bac
  for_ members $ \x -> updateGuildMemberRolesSingle bdt x m bac
  illegalRole <- readProp discordIllegalRole bdt
  when (illegalRole `elem` memberRoles m) $ do
    call_ $ R.RemoveGuildMemberRole gid (userId $ memberUser m) illegalRole
updateDiscordRolesSingle bdt _ _ gid m Nothing = do
  memberRole <- readProp discordMemberRole bdt
  visitorRole <- readProp discordVisitorRole bdt
  divisionRoles <- map snd <$> readProp discordDivisionRoles bdt
  illegalRole <- readProp discordIllegalRole bdt
  when (illegalRole `notElem` memberRoles m && any (`elem` (memberRole:divisionRoles)) (memberRoles m)) $ do
    call_ $ R.AddGuildMemberRole gid (userId $ memberUser m) illegalRole
  when (illegalRole `elem` memberRoles m && all (`notElem` (memberRole:divisionRoles)) (memberRoles m)) $ do
    call_ $ R.RemoveGuildMemberRole gid (userId $ memberUser m) illegalRole
  when (memberRole `notElem` memberRoles m && visitorRole `notElem` memberRoles m) $ do
    call_ $ R.AddGuildMemberRole gid (userId $ memberUser m) visitorRole

updateDiscordRolesSingleId :: BotData -> UserId -> ManagerT DiscordHandler ()
updateDiscordRolesSingleId bdt did = do
  gid <- readProp discordGuildId bdt
  maybeMem <- lift $ call $ R.GetGuildMember gid did
  case maybeMem of
    Left _ -> pure ()
    Right m -> do
      members <- readProp hypixelGuildMembers bdt
      lb <- getHypixelBowLeaderboard
      accs <- (>>=(\u -> (, u) <$> accountDiscords u)) <$> readProp bowBotAccounts bdt
      let bac = lookup did accs
      lift $ updateDiscordRolesSingle bdt lb (if null members then Nothing else Just members) gid m bac

updateRolesAll :: BotData -> ManagerT DiscordHandler ()
updateRolesAll bdt = do
  gid <- readProp discordGuildId bdt
  members <- readProp hypixelGuildMembers bdt
  maybeGmembs <- lift $ fmap (filter (not . userIsBot . memberUser)) <$> call (R.ListGuildMembers gid R.GuildMembersTiming {R.guildMembersTimingLimit = Just 500, R.guildMembersTimingAfter = Nothing})
  lb <- getHypixelBowLeaderboard
  case maybeGmembs of
    Left _ -> pure ()
    Right gmembs -> for_ gmembs $ \m -> do
      accs <- (>>=(\u -> (, u) <$> accountDiscords u)) <$> readProp bowBotAccounts bdt
      let bac = lookup (userId (memberUser m)) accs
      lift $ updateDiscordRolesSingle bdt lb (if null members then Nothing else Just members) gid m bac

getDiscordIds :: APIMonad m => m [UserId]
getDiscordIds = do
  res <- hSendDB "discord/all.php" []
  fmap (fromMaybe []) $ decodeParse res $ \o -> do
    dt <- o .: "data"
    for dt $ \s -> do
      (readMaybe -> Just x) <- pure s
      return x

addDiscords :: BotData -> DiscordHandler ()
addDiscords bdt = do
  manager <- liftIO $ newManager managerSettings
  uids <- runManagerT getDiscordIds manager
  dgid <- readProp discordGuildId bdt
  v <- fmap (filter (not . userIsBot . memberUser)) <$> call (R.ListGuildMembers dgid R.GuildMembersTiming {R.guildMembersTimingLimit = Just 500, R.guildMembersTimingAfter = Nothing})
  case v of
    Right x -> do
      let uids' = filter (\u -> all (\m -> userId (memberUser m) /= u) x) uids
      y <- traverse helper uids'
      runManagerT (updateDiscords x y) manager
    Left x -> logError manager $ show x
  where
    helper :: UserId -> DiscordHandler User
    helper u = do
      y <- call (R.GetUser u)
      return $ fromRight undefined y
    updateDiscords mem usr = hPostDB "discord/update.php" [] (object $ map memToObject mem ++ map usrToObject usr)
       where
         memToObject GuildMember {memberUser = memberUser@User {..}, ..} = case memberNick of
           Nothing -> usrToObject memberUser
           Just nick -> pack (show userId) .= object ["name" .= userName, "discriminator" .= userDiscrim, "nickname" .= nick]
         usrToObject User {..} = pack (show userId) .= object ["name" .= userName, "discriminator" .= userDiscrim]

discordBackgroundMinutely :: BotData -> Int -> DiscordHandler ()
discordBackgroundMinutely bdt mint = do
  when (mint == 1) $ do
    manager <- liftIO $ newManager managerSettings
    hour <- liftIO $ read @Integer <$> getTime "%k"
    when (hour == 5) $ runManagerT (announceBirthdays bdt) manager
    addDiscords bdt
    updateDiscordStatus
    runManagerT (updateRolesAll bdt) manager

-- TODO: frequency updates

completeHypixelBowLeaderboardUpdate :: BotData -> String ->(MinecraftAccount -> Bool) -> IO ()
completeHypixelBowLeaderboardUpdate bdt extra filt = do
  manager <- newManager managerSettings
  mcs <- readProp minecraftAccounts bdt
  let chunked = chunksOf 25 (map mcUUID $ filter filt mcs)
  for_ chunked $ helper manager
    where
      helper manager lst = do
        tryApiRequests (hypixelRequestCounter bdt) 25 (\x -> do { threadDelay ((x+10) * 1000000); helper manager lst }) $ do
          let chunked = chunksOf 10 lst
          dt <- fmap (fromList . catMaybes . zipWith (\a b -> (a,) <$> b) lst . concat) $ for chunked $ mapConcurrently $ fmap (fmap hypixelBowStatsToLeaderboards) . flip runManagerT manager . requestHypixelBowStats
          logInfo' $ show dt
          runManagerT (updateHypixelBowLeaderboard extra dt) manager

clearLogs :: Manager -> IO ()
clearLogs man = do
  website <- fromMaybe "" <$> getEnv "DB_SITE"
  apiKey <- fromMaybe "" <$> getEnv "DB_KEY"
  let url = "http://" ++ website ++ "/api/log/clear.php?key=" ++ apiKey
  request <- parseRequest url
  void $ try @SomeException $ httpLbs request man

backgroundMinutely :: BotData -> Int -> IO ()
backgroundMinutely bdt@BotData {..} mint = do
  atomically $ do
    clearApiRequestCounter hypixelRequestCounter
    clearCache hypixelBowOnlineList
  when (mint == 0) $ do
    logInfo' "started update"
    manager <- newManager managerSettings
    runManagerT (runBotDataT downloadData bdt) manager
    dev <- ifDev False $ return True
    unless dev $ do
      hour <- read @Int <$> getTime "%k"
      when (hour `mod` 8 == 0) $ clearLogs manager
    runManagerT (updateMinecraftAccounts bdt) manager
    logInfo' "finished update"
  when (mint == 30) $ do
    logInfo' "started update"
    hour <- read @Int <$> getTime "%k"
    weekday <- read @Int <$> getTime "%u"
    monthday <- read @Int <$> getTime "%d"
    let extra = case (hour, weekday, monthday) of
          (0, 1, 1) -> "day,week,month"
          (0, 1, _) -> "day,week"
          (0, _, 1) -> "day,month"
          (0, _, _) -> "day"
          _ -> "none"
    when ((hour `mod` 4) == 0) $
      completeHypixelBowLeaderboardUpdate bdt extra $ \MinecraftAccount {..} -> mcHypixelBow == Normal
    logInfo' "finished update"

adminCommands :: [Command]
adminCommands = 
  [ Command "mcrefresh" AdminLevel 120 $ do
          bdt <- hData
          updateMinecraftAccounts bdt
          hRespond "Done"
  , Command "datarefresh" AdminLevel 120 $ do
          downloadData
          hRespond "Done"
  , Command "discordrefresh" AdminLevel 120 $ do
          bdt <- hData
          hDiscord $ addDiscords bdt
          hRespond "Done"
  , Command "rolesrefresh" AdminLevel 120 $ do
          bdt <- hData
          hMDiscord $ updateRolesAll bdt
          hRespond "Done"
  , Command "lbrefresh" AdminLevel 1200 $ do
          bdt <- hData
          liftIO $ completeHypixelBowLeaderboardUpdate bdt "none" $ \MinecraftAccount {..} -> mcHypixelBow /= Banned
          hRespond "Done"
  , Command "lbrefreshday" AdminLevel 1200 $ do
          bdt <- hData
          liftIO $ completeHypixelBowLeaderboardUpdate bdt "day" $ \MinecraftAccount {..} -> mcHypixelBow /= Banned
          hRespond "Done"
  , Command "lbrefreshweek" AdminLevel 1200 $ do
          bdt <- hData
          liftIO $ completeHypixelBowLeaderboardUpdate bdt "day,week" $ \MinecraftAccount {..} -> mcHypixelBow /= Banned
          hRespond "Done"
  , Command "lbrefreshmonth" AdminLevel 1200 $ do
          bdt <- hData
          liftIO $ completeHypixelBowLeaderboardUpdate bdt "day,month" $ \MinecraftAccount {..} -> mcHypixelBow /= Banned
          hRespond "Done"
  , Command "lbrefreshweekmonth" AdminLevel 1200 $ do
          bdt <- hData
          liftIO $ completeHypixelBowLeaderboardUpdate bdt "day,week,month" $ \MinecraftAccount {..} -> mcHypixelBow /= Banned
          hRespond "Done"
  , Command "clearlogs" AdminLevel 120 $ do
          man <- hManager
          liftIO $ clearLogs man
          hRespond "Done"
  , Command "statusrefresh" AdminLevel 120 $ do
          hDiscord updateDiscordStatus
          hRespond "Done"
  , Command "time" AdminLevel 120 $ do
          t <- liftIO $ getTime "Month: %m, Day: %d, Weekday: %u, Hour: %k, Minute: %M, Second %S"
          hRespond t
  ]