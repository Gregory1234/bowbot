{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Data.Either (fromRight)
import Data.List ((\\), intercalate)
import BowBot.Command
import BowBot.Birthday
import Control.Exception.Base (SomeException, try)

announceBirthdays :: (DBMonad m, BotDataMonad m, DiscordMonad m) => m ()
announceBirthdays = do
  currentDay <- liftIO currentBirthdayDate
  birthdays <- getBirthdayPeople currentDay
  dgid <- hRead discordGuildId
  people <- hDiscord $ fmap (filter ((`elem` birthdays) . userId . memberUser)) <$> call (R.ListGuildMembers dgid R.GuildMembersTiming {R.guildMembersTimingLimit = Just 500, R.guildMembersTimingAfter = Nothing})
  case people of
    Left err -> hLogErrorDB $ show err
    Right ppl -> unless (null ppl) $ do
      birthdayChannel <- hRead discordBirthdayChannel
      hLogInfoDB $ "Announcing birthdays: " ++ intercalate ", " (map (showMemberOrUser True . Right) ppl)
      hDiscord $ for_ ppl $ \p -> call $ R.CreateMessage birthdayChannel $ pack $ "**Happy birthday** to " ++ showMemberOrUser True (Right p) ++ "!"

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

updateDivisionRolesSingle :: (DiscordMonad m, BotDataMonad m) => Map UUID HypixelBowLeaderboards -> GuildMember -> BowBotAccount -> m ()
updateDivisionRolesSingle lb memb BowBotAccount { accountMinecrafts = mc } = do
  gid <- hRead discordGuildId
  let did = userId . memberUser $ memb
  let wins = maximum $ mapMaybe (fmap bowLbWins . (lb !?)) mc
  divisionRoles <- hRead discordDivisionRoles
  let currentDivisionRoles = filter (`elem` map snd divisionRoles) (memberRoles memb)
  let targetDivisionRoles = take 1 $ map snd $ filter ((<= wins) . fst) $ reverse divisionRoles
  hDiscord $ for_ (currentDivisionRoles \\ targetDivisionRoles) $ call . R.RemoveGuildMemberRole gid did
  hDiscord $ for_ (targetDivisionRoles \\ currentDivisionRoles) $ call . R.AddGuildMemberRole gid did

updateGuildMemberRolesSingle :: (DiscordMonad m, BotDataMonad m) => [UUID] -> GuildMember -> BowBotAccount -> m ()
updateGuildMemberRolesSingle members memb BowBotAccount { accountMinecrafts = mc } = do
  gid <- hRead discordGuildId
  let did = userId . memberUser $ memb
  let isMember = any (`elem` members) mc
  memberRole <- hRead discordMemberRole
  visitorRole <- hRead discordVisitorRole
  let currentRoles = filter (\x -> x == memberRole || x == visitorRole) (memberRoles memb)
  let targetRoles = [if isMember then memberRole else visitorRole]
  hDiscord $ for_ (currentRoles \\ targetRoles) $ call . R.RemoveGuildMemberRole gid did -- TODO: remove repetition
  hDiscord $ for_ (targetRoles \\ currentRoles) $ call . R.AddGuildMemberRole gid did

-- TODO: remove BotData from here - take it out to a new type
updateDiscordRolesSingle
  :: (DiscordMonad m, BotDataMonad m) => Map UUID HypixelBowLeaderboards -> Maybe [UUID] -> GuildId
  -> GuildMember -> Maybe BowBotAccount -> m ()
updateDiscordRolesSingle lb members gid m (Just bac) = do
  updateDivisionRolesSingle lb m bac
  for_ members $ \x -> updateGuildMemberRolesSingle x m bac
  illegalRole <- hRead discordIllegalRole
  when (illegalRole `elem` memberRoles m) $ do
    hDiscord $ call_ $ R.RemoveGuildMemberRole gid (userId $ memberUser m) illegalRole
updateDiscordRolesSingle _ _ gid m Nothing = do
  memberRole <- hRead discordMemberRole
  visitorRole <- hRead discordVisitorRole
  divisionRoles <- map snd <$> hRead discordDivisionRoles
  illegalRole <- hRead discordIllegalRole
  when (illegalRole `notElem` memberRoles m && any (`elem` (memberRole:divisionRoles)) (memberRoles m)) $ do
    hDiscord $ call_ $ R.AddGuildMemberRole gid (userId $ memberUser m) illegalRole
  when (illegalRole `elem` memberRoles m && all (`notElem` (memberRole:divisionRoles)) (memberRoles m)) $ do
    hDiscord $ call_ $ R.RemoveGuildMemberRole gid (userId $ memberUser m) illegalRole
  when (memberRole `notElem` memberRoles m && visitorRole `notElem` memberRoles m) $ do
    hDiscord $ call_ $ R.AddGuildMemberRole gid (userId $ memberUser m) visitorRole

updateDiscordRolesSingleId :: (DiscordMonad m, DBMonad m, BotDataMonad m) => UserId -> m ()
updateDiscordRolesSingleId did = do
  gid <- hRead discordGuildId
  maybeMem <- hDiscord $ call $ R.GetGuildMember gid did
  case maybeMem of
    Left _ -> pure ()
    Right m -> do
      members <- hRead hypixelGuildMembers
      lb <- getHypixelBowLeaderboard
      accs <- (>>=(\u -> (, u) <$> accountDiscords u)) <$> hRead bowBotAccounts
      let bac = lookup did accs
      updateDiscordRolesSingle lb (if null members then Nothing else Just members) gid m bac

updateRolesAll :: (DiscordMonad m, BotDataMonad m, DBMonad m) => m ()
updateRolesAll = do
  gid <- hRead discordGuildId
  members <- hRead hypixelGuildMembers
  maybeGmembs <- hDiscord $ fmap (filter (not . userIsBot . memberUser)) <$> call (R.ListGuildMembers gid R.GuildMembersTiming {R.guildMembersTimingLimit = Just 500, R.guildMembersTimingAfter = Nothing})
  lb <- getHypixelBowLeaderboard
  case maybeGmembs of
    Left _ -> pure ()
    Right gmembs -> for_ gmembs $ \m -> do
      accs <- (>>=(\u -> (, u) <$> accountDiscords u)) <$> hRead bowBotAccounts
      let bac = lookup (userId (memberUser m)) accs
      updateDiscordRolesSingle lb (if null members then Nothing else Just members) gid m bac

getDiscordIds :: DBMonad m => m [UserId]
getDiscordIds = do
  res :: [Only Integer] <- hQueryLog "SELECT `id` FROM `discordDEV`" ()
  return $ fmap (fromInteger . fromOnly) res

addDiscords :: BotData -> DiscordHandler ()
addDiscords bdt = withDB $ \conn -> do
  uids <- withDB $ runConnectionT getDiscordIds
  dgid <- readProp discordGuildId bdt
  v <- fmap (filter (not . userIsBot . memberUser)) <$> call (R.ListGuildMembers dgid R.GuildMembersTiming {R.guildMembersTimingLimit = Just 500, R.guildMembersTimingAfter = Nothing})
  case v of
    Right x -> do
      let uids' = filter (\u -> all (\m -> userId (memberUser m) /= u) x) uids
      y <- traverse helper uids'
      runConnectionT (updateDiscords x y) conn
    Left x -> logErrorDB conn $ show x
  where
    helper :: UserId -> DiscordHandler User
    helper u = do
      y <- call (R.GetUser u)
      return $ fromRight undefined y
    updateDiscords mem usr = void $ hExecuteManyLog
        "INSERT INTO `discordDEV` (`id`, `name`, `discriminator`, `nickname`) VALUES (?,?,?,?) ON DUPLICATE KEY UPDATE `name`=VALUES(`name`), `discriminator`=VALUES(`discriminator`), `nickname`=VALUES(`nickname`)"
        (map memToObject mem ++ map usrToObject usr)
       where
         memToObject GuildMember {memberUser = User {..}, ..} = (show userId, userName, userDiscrim, memberNick)
         usrToObject User {..} = (show userId, userName, userDiscrim, Nothing)

discordBackgroundMinutely :: BotData -> Int -> DiscordHandler ()
discordBackgroundMinutely bdt mint = do
  when (mint == 1) $ withDB $ \conn -> do
    hour <- liftIO $ read @Integer <$> getTime "%k"
    when (hour == 5) $ runDiscordHandler' $ runConnectionT (runBotDataT announceBirthdays bdt) conn
    addDiscords bdt
    updateDiscordStatus
    runDiscordHandler' $ runConnectionT (runBotDataT updateRolesAll bdt) conn

-- TODO: frequency updates

completeHypixelBowLeaderboardUpdate :: BotData -> [TimeStatsType] -> (MinecraftAccount -> Bool) -> IO ()
completeHypixelBowLeaderboardUpdate bdt extra filt = do
  manager <- newManager managerSettings
  mcs <- readProp minecraftAccounts bdt
  let chunked = chunksOf 25 (map mcUUID $ filter filt mcs)
  withDB $ \conn -> for_ chunked $ helper manager conn
    where
      helper manager conn lst = do
        tryApiRequests (hypixelRequestCounter bdt) 25 (\x -> do { threadDelay ((x+10) * 1000000); helper manager conn lst }) $ do
          let chunked = chunksOf 10 lst
          dt <- fmap (fromList . catMaybes . zipWith (\a b -> (a,) <$> b) lst . concat) $ for chunked $ mapConcurrently $ fmap (fmap hypixelBowStatsToLeaderboards) . flip runManagerT manager . requestHypixelBowStats
          runConnectionT (updateHypixelBowLeaderboard extra dt) conn

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
    withDB $ runConnectionT $ runManagerT (runBotDataT downloadData bdt) manager
    dev <- ifDev False $ return True
    unless dev $ do
      hour <- read @Int <$> getTime "%k"
      when (hour `mod` 8 == 0) $ clearLogs manager
    withDB $ runConnectionT $ runManagerT (updateMinecraftAccounts bdt) manager
    logInfo' "finished update"
  when (mint == 30) $ do
    logInfo' "started update"
    hour <- read @Int <$> getTime "%k"
    weekday <- read @Int <$> getTime "%u"
    monthday <- read @Int <$> getTime "%d"
    let extra = case (hour, weekday, monthday) of
          (0, 1, 1) -> [DailyStats, WeeklyStats, MonthlyStats]
          (0, 1, _) -> [DailyStats, WeeklyStats]
          (0, _, 1) -> [DailyStats, MonthlyStats]
          (0, _, _) -> [DailyStats]
          _ -> []
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
          updateRolesAll
          hRespond "Done"
  , Command "lbrefresh" AdminLevel 1200 $ do
          bdt <- hData
          liftIO $ completeHypixelBowLeaderboardUpdate bdt [] $ \MinecraftAccount {..} -> mcHypixelBow /= Banned
          hRespond "Done"
  , Command "lbrefreshday" AdminLevel 1200 $ do
          bdt <- hData
          liftIO $ completeHypixelBowLeaderboardUpdate bdt [DailyStats] $ \MinecraftAccount {..} -> mcHypixelBow /= Banned
          hRespond "Done"
  , Command "lbrefreshweek" AdminLevel 1200 $ do
          bdt <- hData
          liftIO $ completeHypixelBowLeaderboardUpdate bdt [DailyStats, WeeklyStats] $ \MinecraftAccount {..} -> mcHypixelBow /= Banned
          hRespond "Done"
  , Command "lbrefreshmonth" AdminLevel 1200 $ do
          bdt <- hData
          liftIO $ completeHypixelBowLeaderboardUpdate bdt [DailyStats, MonthlyStats] $ \MinecraftAccount {..} -> mcHypixelBow /= Banned
          hRespond "Done"
  , Command "lbrefreshweekmonth" AdminLevel 1200 $ do
          bdt <- hData
          liftIO $ completeHypixelBowLeaderboardUpdate bdt [DailyStats, WeeklyStats, MonthlyStats] $ \MinecraftAccount {..} -> mcHypixelBow /= Banned
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