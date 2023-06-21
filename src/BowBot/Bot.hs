module BowBot.Bot where

import BowBot.Utils
import Discord
import Discord.Requests
import Discord.Types
import BowBot.DB.Basic
import BowBot.BotData.Info
import BowBot.BotMonad
import qualified Data.Text as T
import BowBot.Command
import BowBot.Hypixel.StatsCommand
import BowBot.Discord.Basic
import Control.Exception.Base (SomeException, try, throw)
import System.Timeout (timeout)
import BowBot.BotData.Download
import BowBot.BotData.RefreshCommand
import BowBot.BotData.Cached
import Control.Concurrent (threadDelay, forkIO)
import BowBot.Settings.Basic
import BowBot.Network.Basic
import BowBot.Network.ClearLogs
import BowBot.Discord.Roles
import BowBot.Discord.SavedRoles
import BowBot.Hypixel.LeaderboardCommand
import BowBot.Hypixel.TimeStats
import BowBot.Hypixel.WatchlistCommands
import BowBot.Discord.Account
import BowBot.Account.Basic
import BowBot.Command.HelpCommand
import BowBot.Minecraft.Basic
import BowBot.Minecraft.Account
import BowBot.Minecraft.UrlCommand
import BowBot.Minecraft.NameCommand
import BowBot.Account.InfoCommand
import BowBot.Account.RegisterCommand
import BowBot.Discord.RoleCommand
import BowBot.Hypixel.BanCommand
import BowBot.Settings.Commands
import BowBot.Minecraft.SelectCommand
import BowBot.Birthday.Announce
import BowBot.Birthday.SetCommand
import BowBot.Snipe.Detect
import BowBot.Snipe.Command
import BowBot.Hypixel.Announce
import BowBot.BotData.Basic
import BowBot.Counter.Basic
import BowBot.Hypixel.Guild

runBowBot :: IO ()
runBowBot = do
  discordKey <- getEnvOrThrow "API_KEY"
  ifDev () $ putStrLn "this is dev version of the bot"
  bctxManager <- newManager managerSettings
  bctxCounter <- atomically newCounterState
  bctxData <- downloadBotData
  bctxInfo <- downloadInfoCache
  logInfoFork "bot started"
  userFacingError <-
    runDiscord $
      def
        { discordToken = pack discordKey,
          discordOnStart = ReaderT $ onStartup bctxManager bctxCounter bctxData bctxInfo,
          discordOnEvent = \e -> ReaderT $ \bctxDiscord -> withDB $ \bctxConnection -> runReaderT (eventHandler e) BotContext {..},
          discordOnLog = putStrLn . unpack,
          discordGatewayIntent = def { gatewayIntentMembers = True }
        }
  logErrorFork userFacingError

backgroundMinutely :: Int -> Bot ()
backgroundMinutely mint = do
  clearBotDataCaches
  when (mint == 0) $ do
    logInfo "started update"
    updateDiscordStatus
    refreshInfoCache
    refreshBotData
    hour <- liftIO $ read @Int <$> getTime "%k"
    weekday <- liftIO $ read @Int <$> getTime "%u"
    monthday <- liftIO $ read @Int <$> getTime "%d"
    let times = case (hour, weekday, monthday) of
          (0, 1, 1) -> [DailyStats, WeeklyStats, MonthlyStats]
          (0, 1, _) -> [DailyStats, WeeklyStats]
          (0, _, 1) -> [DailyStats, MonthlyStats]
          (0, _, _) -> [DailyStats]
          _ -> []
    when (hour == 5) announceBirthdays
    updateBotData times
    announceMilestones
    updateSavedRolesAll
    applyRolesAll
    updateMinecraftAccountCache hour
    dev <- ifDev False $ return True
    unless dev $ when (hour `mod` 8 == 0) clearLogs
    logInfo "finished update"

onStartup :: Manager -> CounterState -> BotData -> InfoCache -> DiscordHandle -> IO ()
onStartup bctxManager bctxCounter bctxData bctxInfo bctxDiscord = void $ forkIO $ do
  withDB $ \bctxConnection -> runReaderT updateDiscordStatus BotContext {..}
  sec <- read @Int <$> getTime "%S"
  liftIO $ threadDelay ((65 - sec `mod` 60) * 1000000)
  void $ forever $ do
    _ <- forkIO $ backgroundTimeoutRun 6000 $ do
      mint <- liftIO $ read @Int <$> getTime "%M"
      withDB $ \bctxConnection -> runReaderT (backgroundMinutely mint) BotContext {..}
    liftIO $ threadDelay 60000000

updateDiscordStatus :: (MonadIOReader m r, HasAll '[DiscordHandle, InfoCache] r) => m ()
updateDiscordStatus = do
  discordStatus <- askInfo discordStatusInfo
  liftDiscord $ sendCommand (UpdateStatus $ UpdateStatusOpts {
        updateStatusOptsSince = Nothing,
        updateStatusOptsGame = Just (def {activityName = discordStatus}),
        updateStatusOptsNewStatus = UpdateStatusOnline,
        updateStatusOptsAFK = False
      })

respond' :: (MonadIOReader m r, Has DiscordHandle r) => Message -> Text -> m ()
respond' m = call_ . CreateMessage (messageChannelId m)

eventHandler :: Event -> Bot ()
eventHandler (MessageCreate m) = do
  detectDeleteMessage m
  -- liftIO $ detectRTWData man bdt m
  unless (userIsBot (messageAuthor m)) $ do
    prefix <- askInfo discordCommandPrefixInfo
    when (prefix `T.isPrefixOf` messageContent m) $ do
      let n = T.toLower . T.drop (T.length prefix) . T.takeWhile (/= ' ') $ messageContent m
      for_ (filter ((==n) . commandName . commandInfo) commands) $ \c ->
        commandTimeoutRun (commandTimeout $ commandInfo c) m $ do
          logInfo $ "recieved " <> messageContent m
          ifDev () $ do
            testDiscordId <- askInfo discordGuildIdInfo
            when (messageGuildId m /= Just testDiscordId) $
              respond' m "```Attention! This is the dev version of the bot! Some features might not be avaliable! You shouldn't be reading this! If you see this message please report it immidately!```"
          perms <- getPermissionLevelByDiscord (userId (messageAuthor m))
          if perms == BanLevel
          then respond' m "You have been blacklisted. You can probably appeal this decision. Or not. I don't know. I'm just a pre-programmed response."
          else if perms >= commandPerms (commandInfo c)
            then runCommand c m
            else respond' m "You don't have the permission to do that!"
          logInfo $ "finished " <> messageContent m
eventHandler (GuildMemberAdd gid gmem) = do
  maingid <- askInfo discordGuildIdInfo
  when (gid == maingid && not (maybe True userIsBot (memberUser gmem))) $ do
    storeDiscordAccount $ guildMemberToDiscordAccount gmem
    applyRoles gmem
eventHandler (GuildMemberUpdate gid roles usr newname) = do
  maingid <- askInfo discordGuildIdInfo
  when (gid == maingid && not (userIsBot usr)) $ do
    storeDiscordAccount $ let acc = userToDiscordAccount usr in acc { discordName = (discordName acc) { discordNickname = newname <|> discordNickname (discordName acc) }, discordIsMember = True }
    unless (null roles) $ do
      savedRoles <- savedRolesFromIds roles
      setSavedRolesByDiscord (userId usr) savedRoles
eventHandler (GuildMemberRemove gid usr) = do
  maingid <- askInfo discordGuildIdInfo
  when (gid == maingid && not (userIsBot usr)) $ do
    storeDiscordAccount $ userToDiscordAccount usr
eventHandler _ = pure ()

commandTimeoutRun :: (MonadHoistIO m, MonadReader r m, Has DiscordHandle r) => Int -> Message -> m () -> m ()
commandTimeoutRun n msg x = do
  tm <- hoistIO (try @SomeException . timeout (n * 1000000)) x
  case tm of
    Left e -> do
      logErrorFork $ "Exception happened in command: " <> showt e
      respond' msg "Something went horribly wrong! Please report this!"
      throw e
    Right Nothing -> do
      logErrorFork $ "Timed out: " <> showt n <> "s"
      respond' msg "Timed out! Please report this!"
    Right (Just ()) -> pure ()

backgroundTimeoutRun :: MonadHoistIO m => Int -> m () -> m ()
backgroundTimeoutRun n x = do
  tm <- hoistIO (try @SomeException . timeout (n * 1000000)) x
  case tm of
    Left e -> do
      logErrorFork $ "Exception happened in command: " <> showt e
      throw e
    Right Nothing -> do
      logErrorFork $ "Timed out: " <> showt n <> "s"
    Right (Just ()) -> pure ()

commands :: [Command]
commands =
  [ helpCommand commands DefaultLevel Nothing "normal" "help"
  , registerCommand
  , hypixelStatsCommand UserSettings "s" "show player's Bow Duels stats"
  , hypixelStatsCommand DefSettings "sd" "show a default set of player's Bow Duels stats"
  , hypixelStatsCommand AllSettings "sa" "show all of player's Bow Duels stats"
  , hypixelTimeStatsCommand UserSettings "st" "show daily, weekly and monthly Bow Duels stats"
  , leaderboardCommand winsLeaderboardType "lb"
  , leaderboardCommand lossesLeaderboardType "lbl"
  , leaderboardCommand winstreakLeaderboardType "lbs"
  , leaderboardCommand wlrLeaderboardType "lbr"
  , leaderboardCommand winsLeaderboardTypeGuild "lbg"
  , leaderboardCommand lossesLeaderboardTypeGuild "lblg"
  , leaderboardCommand winstreakLeaderboardTypeGuild "lbsg"
  , leaderboardCommand wlrLeaderboardTypeGuild "lbrg"
  , infoCommand
  , snipeCommand
  , roleCommand
  , nameCommand
  , urlCommand "head" "show player's head" $ \s -> "https://crafatar.com/avatars/" <> uuidString s <> "?overlay"
  , urlCommand "skin" "show player's full skin" $ \s -> "https://crafatar.com/renders/body/" <> uuidString s <> "?overlay"
  , listCommand
  , onlineCommand
  , helpCommand commands DefaultLevel (Just $ \prefix -> "*Visibility 'maybe' and 'defined' hide the stat when the value is undefined.*\n"
                                                <> "**Stat names:** wins, losses, wlr, winsuntil, beststreak, currentstreak, bestdailystreak, bowhits, bowshots, accuracy\n"
                                                <> "**Example:** `" <> prefix <> "show accuracy` makes accuracy visible in the `" <> prefix <> "s` command\n" ) "settings" "settings"
  , setSettingCommand
  , constSettingCommand Yes Always "show" "makes the stat visible"
  , constSettingCommand No Never "hide" "makes the stat hidden"
  , selectMinecraftCommand
  , helpCommand commands ModLevel Nothing "normal" "modhelp"
  , addCommand
  , hypixelBanCommand
  , setBirthdayCommand
  , helpCommand commands AdminLevel Nothing "normal" "adminhelp"
  , adminCommand 30 "datarefresh" "sync Bow Bot's data from the database" refreshBotData
  , adminCommand 30 "inforefresh" "sync Bow Bot's info cache from the database" refreshInfoCache
  , adminCommand 3600 "discordupdate" "update discord data" updateDiscordAccountCache
  , updateDataCommand [] "dataupdate"
  , updateDataCommand [DailyStats] "dataupdateday"
  , updateDataCommand [DailyStats, WeeklyStats] "dataupdateweek"
  , updateDataCommand [DailyStats, MonthlyStats] "dataupdatemonth"
  , updateDataCommand [DailyStats, WeeklyStats, MonthlyStats] "dataupdateweekmonth"
  , updateNamesCommand
  , adminCommand 15 "clearlogs" "clear Bow Bot's logs" clearLogs
  , adminCommand 120 "rolesupdate" "update everyone's discord roles" $ do { updateHypixelRoles; applyRolesAll }
  , adminCommand 120 "savedrolesstore" "store everyone's saved roles" updateSavedRolesAll
  , adminCommand 15 "statusupdate" "update Bow Bot's discord status" updateDiscordStatus
  , quietAdminCommand 5 "throw" "throw an error" $ respond $ showt ((1 :: Integer) `div` 0)
  , quietAdminCommand 5 "time" "display Bow Bot's time" $ respond . pack =<< liftIO (getTime "Month: %m, Day: %d, Weekday: %u, Hour: %k, Minute: %M, Second %S")
  , adminCommand 15 "bdsay" "announce today's birthdays" announceBirthdays
  , quietAdminCommand 5 "evacuate" "leaves the discord server" $ envs envGuild >>= call_ . LeaveGuild
  , Command CommandInfo { commandName = "gregc", commandHelpEntries = [], commandPerms = DefaultLevel, commandTimeout = 2 } $ noArguments $ respond "<:gregc:904127204865228851>"
  ]