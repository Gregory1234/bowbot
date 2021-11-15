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
import Data.Proxy
import BowBot.Stats
import BowBot.API
import Network.HTTP.Conduit (newManager, httpLbs, parseRequest)
import Data.List.Split (chunksOf)
import Control.Concurrent (threadDelay)
import Data.Map ((!?))
import Control.Concurrent.Async (mapConcurrently)
import Data.Maybe (catMaybes, mapMaybe)
import Discord.Types hiding (accountId)
import Data.Aeson.Types (object, (.=))
import Data.Either (fromRight)
import Data.List ((\\))
import BowBot.Command
import Control.Exception.Base (SomeException, try, evaluate)

updateDiscordStatus :: Manager -> DiscordHandler ()
updateDiscordStatus man = do
  status <- liftIO $ getInfoDB man "discord_status"
  sendCommand (UpdateStatus $ UpdateStatusOpts {
      updateStatusOptsSince = Nothing,
      updateStatusOptsGame = case status of
         Nothing -> Nothing
         Just s -> Just (Activity {activityName = pack s, activityType = ActivityTypeGame, activityUrl = Nothing}),
      updateStatusOptsNewStatus = UpdateStatusOnline,
      updateStatusOptsAFK = False
    })

updateDivisionRolesSingle :: BotData -> Map String (Leaderboards HypixelBowStats) -> GuildMember -> BowBotAccount -> DiscordHandler ()
updateDivisionRolesSingle bdt lb memb BowBotAccount { accountMinecrafts = mc } = do
  gid <- liftIO $ atomically $ readTVar (discordGuildId bdt)
  let did = userId . memberUser $ memb
  let wins = maximum $ mapMaybe (fmap bowLbWins . (lb !?)) mc
  divisionRoles <- liftIO $ atomically $ readTVar (discordDivisionRoles bdt)
  let currentDivisionRoles = filter (`elem` map snd divisionRoles) (memberRoles memb)
  let targetDivisionRoles = take 1 $ map snd $ filter ((<= wins) . fst) $ reverse divisionRoles
  for_ (currentDivisionRoles \\ targetDivisionRoles) $ call . R.RemoveGuildMemberRole gid did
  for_ (targetDivisionRoles \\ currentDivisionRoles) $ call . R.AddGuildMemberRole gid did

updateGuildMemberRolesSingle :: BotData -> [String] -> GuildMember -> BowBotAccount -> DiscordHandler ()
updateGuildMemberRolesSingle bdt members memb BowBotAccount { accountMinecrafts = mc } = do
  gid <- liftIO $ atomically $ readTVar (discordGuildId bdt)
  let did = userId . memberUser $ memb
  let isMember = any (`elem` members) mc
  memberRole <- liftIO $ atomically $ readTVar (discordMemberRole bdt)
  visitorRole <- liftIO $ atomically $ readTVar (discordVisitorRole bdt)
  let currentRoles = filter (\x -> x == memberRole || x == visitorRole) (memberRoles memb)
  let targetRoles = [if isMember then memberRole else visitorRole]
  for_ (currentRoles \\ targetRoles) $ call . R.RemoveGuildMemberRole gid did -- TODO: remove repetition
  for_ (targetRoles \\ currentRoles) $ call . R.AddGuildMemberRole gid did

-- TODO: remove BotData from here - take it out to a new type
updateDiscordRolesSingle
  :: BotData -> Maybe (Map String (Leaderboards HypixelBowStats)) -> Maybe [String] -> GuildId
  -> GuildMember -> Maybe BowBotAccount -> DiscordHandler ()
updateDiscordRolesSingle bdt lb members gid m (Just bac) = do
  for_ lb $ \x -> updateDivisionRolesSingle bdt x m bac
  for_ members $ \x -> updateGuildMemberRolesSingle bdt x m bac
  illegalRole <- liftIO $ atomically $ readTVar (discordIllegalRole bdt)
  when (illegalRole `elem` memberRoles m) $ do
    call_ $ R.RemoveGuildMemberRole gid (userId $ memberUser m) illegalRole
updateDiscordRolesSingle bdt _ _ gid m Nothing = do
  memberRole <- liftIO $ atomically $ readTVar (discordMemberRole bdt)
  visitorRole <- liftIO $ atomically $ readTVar (discordVisitorRole bdt)
  divisionRoles <- map snd <$> liftIO (atomically $ readTVar (discordDivisionRoles bdt))
  illegalRole <- liftIO $ atomically $ readTVar (discordIllegalRole bdt)
  when (illegalRole `notElem` memberRoles m && any (`elem` (memberRole:divisionRoles)) (memberRoles m)) $ do
    call_ $ R.AddGuildMemberRole gid (userId $ memberUser m) illegalRole
  when (illegalRole `elem` memberRoles m && all (`notElem` (memberRole:divisionRoles)) (memberRoles m)) $ do
    call_ $ R.RemoveGuildMemberRole gid (userId $ memberUser m) illegalRole
  when (memberRole `notElem` memberRoles m && visitorRole `notElem` memberRoles m) $ do
    call_ $ R.AddGuildMemberRole gid (userId $ memberUser m) visitorRole

updateDiscordRolesSingleId :: BotData -> Manager -> UserId -> DiscordHandler ()
updateDiscordRolesSingleId bdt man did = do
  gid <- liftIO $ atomically $ readTVar (discordGuildId bdt)
  maybeMem <- call $ R.GetGuildMember gid did
  case maybeMem of
    Left _ -> pure ()
    Right m -> do
      members <- liftIO $ atomically $ readTVar (hypixelGuildMembers bdt)
      lb <- liftIO $ getLeaderboard (Proxy @HypixelBowStats) man
      accs <- liftIO $ fmap (>>=(\u -> (, u) <$> accountDiscords u)) $ atomically $ readTVar $ bowBotAccounts bdt
      let bac = lookup did accs
      updateDiscordRolesSingle bdt lb (if null members then Nothing else Just members) gid m bac

updateRolesAll :: BotData -> Manager -> DiscordHandler ()
updateRolesAll bdt man = do
  gid <- liftIO $ atomically $ readTVar (discordGuildId bdt)
  members <- liftIO $ atomically $ readTVar (hypixelGuildMembers bdt)
  maybeGmembs <- fmap (filter (not . userIsBot . memberUser)) <$> call (R.ListGuildMembers gid R.GuildMembersTiming {R.guildMembersTimingLimit = Just 500, R.guildMembersTimingAfter = Nothing})
  lb <- liftIO $ getLeaderboard (Proxy @HypixelBowStats) man
  case maybeGmembs of
    Left _ -> pure ()
    Right gmembs -> for_ gmembs $ \m -> do
      accs <- liftIO $ fmap (>>=(\u -> (, u) <$> accountDiscords u)) $ atomically $ readTVar $ bowBotAccounts bdt
      let bac = lookup (userId (memberUser m)) accs
      updateDiscordRolesSingle bdt lb (if null members then Nothing else Just members) gid m bac

getDiscordIds :: Manager -> IO [UserId]
getDiscordIds manager = do
  res <- sendDB manager "discord/all.php" []
  fmap (fromMaybe []) $ decodeParse res $ \o -> do
    dt <- o .: "data"
    for dt $ \s -> do
      (readMaybe -> Just x) <- pure s
      return x

addDiscords :: BotData -> DiscordHandler ()
addDiscords bdt = do
  manager <- liftIO $ newManager managerSettings
  uids <- liftIO $ getDiscordIds manager
  dgid <- liftIO $ atomically $ readTVar (discordGuildId bdt)
  v <- fmap (filter (not . userIsBot . memberUser)) <$> call (R.ListGuildMembers dgid R.GuildMembersTiming {R.guildMembersTimingLimit = Just 500, R.guildMembersTimingAfter = Nothing})
  case v of
    Right x -> do
      let uids' = filter (\u -> all (\m -> userId (memberUser m) /= u) x) uids
      y <- traverse helper uids'
      liftIO $ updateDiscords manager x y
    Left x -> logError manager $ show x
  where
    helper :: UserId -> DiscordHandler User
    helper u = do
      y <- call (R.GetUser u)
      return $ fromRight undefined y
    updateDiscords manager mem usr = sendPostDB manager "discord/update.php" (object $ map memToObject mem ++ map usrToObject usr)
       where
         memToObject GuildMember {memberUser = memberUser@User {..}, ..} = case memberNick of
           Nothing -> usrToObject memberUser
           Just nick -> pack (show userId) .= object ["name" .= userName, "discriminator" .= userDiscrim, "nickname" .= nick]
         usrToObject User {..} = pack (show userId) .= object ["name" .= userName, "discriminator" .= userDiscrim]

discordBackgroundMinutely :: BotData -> Int -> DiscordHandler ()
discordBackgroundMinutely bdt mint = do
  when (mint == 1) $ do
    addDiscords bdt
    manager <- liftIO $ newManager managerSettings
    updateDiscordStatus manager
    updateRolesAll bdt manager

-- TODO: frequency updates

completeLeaderboardUpdate :: StatType s => Proxy s -> BotData -> ApiRequestCounter -> (MinecraftAccount -> Bool) -> IO ()
completeLeaderboardUpdate pr bdt api filt = do
  manager <- newManager managerSettings
  mcs <- atomically $ readTVar $ minecraftAccounts bdt
  let chunked = chunksOf 25 (map mcUUID $ filter filt mcs)
  for_ chunked $ helper manager
    where
      helper manager lst = do
        tryApiRequests api 25 (\x -> do { threadDelay ((x+10) * 1000000); helper manager lst }) $ do
          let chunked = chunksOf 10 lst
          dt <- fmap (fromList . catMaybes . zipWith (\a b -> (a,) <$> b) lst . concat) $ for chunked $ mapConcurrently $ fmap (fmap toLeaderboard) . requestStats pr manager
          logInfo' $ show dt
          updateLeaderboard manager dt

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
    downloadData bdt
    manager <- newManager managerSettings
    dev <- ifDev False $ return True
    unless dev $ do
      hour <- read @Int <$> getTime "%k"
      when (hour `mod` 8 == 0) $ clearLogs manager
    updateMinecraftAccounts bdt manager
    logInfo' "finished update"
  when (mint == 30) $ do
    logInfo' "started update"
    hour <- read @Int <$> getTime "%k"
    weekday <- read @Int <$> getTime "%u"
    when (even hour) $
      completeLeaderboardUpdate (Proxy @HypixelBowStats) bdt hypixelRequestCounter $ \MinecraftAccount {..} -> mcHypixelBow == BiHourly
    when (hour == 0) $
      completeLeaderboardUpdate (Proxy @HypixelBowStats) bdt hypixelRequestCounter $ \MinecraftAccount {..} -> mcHypixelBow == Daily
    when (weekday == 1) $
      completeLeaderboardUpdate (Proxy @HypixelBowStats) bdt hypixelRequestCounter $ \MinecraftAccount {..} -> mcHypixelBow == Weekly
    logInfo' "finished update"

adminCommands :: [Command]
adminCommands = 
  [ Command "refresh" AdminLevel 120 $ \m _ bdt -> do
          liftIO $ downloadData bdt
          manager <- liftIO $ newManager managerSettings
          liftIO $ updateMinecraftAccounts bdt manager
          respond m "Done"
  , Command "discordrefresh" AdminLevel 120 $ \m _ bdt -> do
          addDiscords bdt
          respond m "Done"
  , Command "rolesrefresh" AdminLevel 120 $ \m man bdt -> do
          updateRolesAll bdt man
          respond m "Done"
  , Command "lbrefresh" AdminLevel 1200 $ \m _ bdt -> do
          liftIO $ completeLeaderboardUpdate (Proxy @HypixelBowStats) bdt (hypixelRequestCounter bdt) $ \MinecraftAccount {..} -> mcHypixelBow /= Banned
          respond m "Done"
  , Command "clearlogs" AdminLevel 120 $ \m man _ -> do
          liftIO $ clearLogs man
          respond m "Done"
  , Command "statusrefresh" AdminLevel 120 $ \m man _ -> do
          updateDiscordStatus man
          respond m "Done"
  ]