{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.Bot where

import BowBot.Utils
import Discord
import Discord.Requests
import Discord.Types
import Control.Monad (forever)
import BowBot.DB.Basic
import BowBot.BotData.Info
import Network.HTTP.Conduit (newManager)
import BowBot.Network.Basic (managerSettings)
import BowBot.BotMonad
import qualified Data.Text as T
import Data.Text (isPrefixOf)
import BowBot.Command
import BowBot.Hypixel.StatsCommand
import BowBot.Discord.Class (MonadDiscord, call_, liftDiscord)
import Control.Exception.Base (SomeException, try, throw)
import System.Timeout (timeout)
import Control.Monad.Reader (ReaderT(..))
import BowBot.BotData.Download
import BowBot.BotData.RefreshCommand
import Data.Proxy
import BowBot.BotData.Cached
import Control.Concurrent (threadDelay, forkIO)
import BowBot.Settings.Basic
import BowBot.Network.Class (hManager)
import BowBot.Network.ClearLogs
import BowBot.Discord.Roles
import BowBot.Hypixel.LeaderboardCommand
import BowBot.Hypixel.TimeStats
import BowBot.Hypixel.TimeStatsCommand
import BowBot.Hypixel.WatchlistCommands
import BowBot.Discord.Account
import BowBot.Account.Basic
import BowBot.Command.HelpCommand
import BowBot.Minecraft.Basic
import BowBot.Minecraft.UrlCommand
import BowBot.Minecraft.NameCommand
import BowBot.Account.InfoCommand
import BowBot.Account.RegisterCommand
import BowBot.Discord.RoleCommand
import BowBot.Hypixel.BanCommand
import BowBot.Settings.Commands
import BowBot.Minecraft.SelectCommand

runBowBot :: IO ()
runBowBot = do
  discordKey <- getEnvOrThrow "API_KEY"
  ifDev () $ putStrLn "this is dev version of the bot"
  manager <- newManager managerSettings
  bdt <- downloadBotData
  logInfo "bot started"
  userFacingError <-
    runDiscord $
      def
        { discordToken = pack discordKey,
          discordOnStart = ReaderT $ runBotT onStartup bdt manager,
          discordOnEvent = \e -> ReaderT $ runBotT (eventHandler e) bdt manager,
          discordOnLog = putStrLn . unpack,
          discordGatewayIntent = def { gatewayIntentMembers = True }
        }
  logError $ unpack userFacingError

backgroundMinutely :: Int -> Bot ()
backgroundMinutely mint = do
  bdt <- BotT $ \d _ _ -> return d
  liftIO $ clearBotDataCaches bdt
  when (mint == 0) $ withDB $ \conn -> do
    logInfoDB conn "started update"
    updateDiscordStatus
    liftIO $ refreshBotData conn bdt
    manager <- hManager
    hour <- liftIO $ read @Int <$> getTime "%k"
    weekday <- liftIO $ read @Int <$> getTime "%u"
    monthday <- liftIO $ read @Int <$> getTime "%d"
    let times = case (hour, weekday, monthday) of
          (0, 1, 1) -> [DailyStats, WeeklyStats, MonthlyStats]
          (0, 1, _) -> [DailyStats, WeeklyStats]
          (0, _, 1) -> [DailyStats, MonthlyStats]
          (0, _, _) -> [DailyStats]
          _ -> []
    liftDiscord $ updateBotData times manager bdt
    updateRolesAll
    dev <- ifDev False $ return True
    unless dev $ when (hour `mod` 8 == 0) clearLogs
    logInfoDB conn "finished update"

onStartup :: Bot ()
onStartup = void $ hoistIO forkIO $ do
  updateDiscordStatus
  sec <- liftIO $ read @Int <$> getTime "%S"
  liftIO $ threadDelay ((65 - sec `mod` 60) * 1000000)
  void $ forever $ do
    _ <- hoistIO forkIO $ backgroundTimeoutRun 6000 $ do
      mint <- liftIO $ read @Int <$> getTime "%M"
      backgroundMinutely mint
    liftIO $ threadDelay 60000000

updateDiscordStatus :: (MonadDiscord m, MonadCache InfoField m) => m ()
updateDiscordStatus = do
  discordStatus <- hInfoDB discordStatusInfo
  liftDiscord $ sendCommand (UpdateStatus $ UpdateStatusOpts {
        updateStatusOptsSince = Nothing,
        updateStatusOptsGame = Just (def {activityName = pack discordStatus}),
        updateStatusOptsNewStatus = UpdateStatusOnline,
        updateStatusOptsAFK = False
      })

respond :: MonadDiscord m => Message -> String -> m ()
respond m = call_ . CreateMessage (messageChannelId m) . pack

eventHandler :: Event -> Bot ()
eventHandler (MessageCreate m) = do
  -- liftIO $ runBotDataT (detectDeleteMessage m) bdt
  -- liftIO $ detectRTWData man bdt m
  unless (userIsBot (messageAuthor m)) $ do
    prefix <- hInfoDB discordCommandPrefixInfo
    when (pack prefix `isPrefixOf` messageContent m) $ do
      let n = unpack $ T.toLower . T.drop (length prefix) . T.takeWhile (/= ' ') $ messageContent m
      for_ (filter ((==n) . commandName . commandInfo) commands) $ \c ->
        commandTimeoutRun (commandTimeout $ commandInfo c) m $ do
          logInfo $ "recieved " ++ unpack (messageContent m)
          ifDev () $ do
            testDiscordId <- hInfoDB discordGuildIdInfo
            when (messageGuildId m /= Just testDiscordId) $
              respond m "```Attention! This is the dev version of the bot! Some features might not be avaliable! You shouldn't be reading this! If you see this message please report it immidately!```"
          perms <- fromMaybe DefaultLevel <$> getFromCache (Proxy @PermissionLevel) (userId (messageAuthor m))
          if perms == BanLevel
          then respond m "You have been blacklisted. You can probably appeal this decision. Or not. I don't know. I'm just a pre-programmed response."
          else if perms >= commandPerms (commandInfo c)
            then runCommand c m
            else respond m "You don't have the permission to do that!"
          logInfo $ "finished " ++ unpack (messageContent m)
eventHandler (GuildMemberAdd gid gmem) = do
  maingid <- hInfoDB discordGuildIdInfo
  when (gid == maingid && not (maybe True userIsBot (memberUser gmem))) $ do
    void $ storeInCache [guildMemberToDiscordAccount gmem]
    acc <- getBowBotAccountByDiscord (maybe 0 userId (memberUser gmem))
    updateRoles gmem acc
eventHandler (GuildMemberUpdate gid roles usr _) = do
  maingid <- hInfoDB discordGuildIdInfo
  when (not (null roles) && gid == maingid && not (userIsBot usr)) $ do
    storeNewRolesSaved (userId usr) roles
eventHandler _ = pure ()

commandTimeoutRun :: (MonadHoistIO m, MonadDiscord m) => Int -> Message -> m () -> m ()
commandTimeoutRun n msg x = do
  tm <- hoistIO (try @SomeException . timeout (n * 1000000)) x
  case tm of
    Left e -> do
      logError $ "Exception happened in command: " ++ show e
      respond msg "Something went horribly wrong! Please report this!"
      throw e
    Right Nothing -> do
      logError $ "Timed out: " ++ show n ++ "s"
      respond msg "Timed out! Please report this!"
    Right (Just ()) -> pure ()

backgroundTimeoutRun :: MonadHoistIO m => Int -> m () -> m ()
backgroundTimeoutRun n x = do
  tm <- hoistIO (try @SomeException . timeout (n * 1000000)) x
  case tm of
    Left e -> do
      logError $ "Exception happened in command: " ++ show e
      throw e
    Right Nothing -> do
      logError $ "Timed out: " ++ show n ++ "s"
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
  , infoCommand
  , roleCommand
  , nameCommand
  , urlCommand "head" "show player's head" $ \s -> "https://crafatar.com/avatars/" ++ uuidString s ++ "?overlay"
  , urlCommand "skin" "show player's full skin" $ \s -> "https://crafatar.com/renders/body/" ++ uuidString s ++ "?overlay"
  , listCommand
  , onlineCommand
  , helpCommand commands DefaultLevel (Just $ \prefix -> "*Visibility 'maybe' and 'defined' hide the stat when the value is undefined.*\n"
                                                ++ "**Stat names:** wins, losses, wlr, winsuntil, beststreak, currentstreak, bestdailystreak, bowhits, bowshots, accuracy\n"
                                                ++ "**Example:** `" ++ prefix ++ "show accuracy` makes accuracy visible in the `" ++ prefix ++ "s` command\n" ) "settings" "settings"
  , setSettingCommand
  , constSettingCommand True Always "show" "makes the stat visible"
  , constSettingCommand False Never "hide" "makes the stat hidden"
  , selectMinecraftCommand
  , helpCommand commands ModLevel Nothing "normal" "modhelp"
  , addCommand
  , addaltCommand
  , hypixelBanCommand
  , helpCommand commands AdminLevel Nothing "normal" "adminhelp"
  , adminCommand "datarefresh" "sync Bow Bot's data from the database" $ \bdt -> liftIO $ withDB $ \conn -> refreshBotData conn bdt
  , updateDataCommand [] "dataupdate"
  , updateDataCommand [DailyStats] "dataupdateday"
  , updateDataCommand [DailyStats, WeeklyStats] "dataupdateweek"
  , updateDataCommand [DailyStats, MonthlyStats] "dataupdatemonth"
  , updateDataCommand [DailyStats, WeeklyStats, MonthlyStats] "dataupdateweekmonth"
  , adminCommand "clearLogs" "clear Bow Bot's logs" $ const clearLogs
  , adminCommand "rolesupdate" "update everyone's discord roles" $ const updateRolesAll
  , adminCommand "savedrolesstore" "store everyone's saved roles" $ const storeNewSavedRolesAll
  , adminCommand "statusupdate" "update Bow Bot's discord status"$ const updateDiscordStatus
  , quietAdminCommand "throw" "throw an error" $ const $ hRespond $ show ((1 :: Integer) `div` 0)
  , quietAdminCommand "time" "display Bow Bot's time" $ const $ hRespond =<< liftIO (getTime "Month: %m, Day: %d, Weekday: %u, Hour: %k, Minute: %M, Second %S")
  , Command CommandInfo { commandName = "gregc", commandHelpEntries = [], commandPerms = DefaultLevel, commandTimeout = 2 } $ hNoArguments $ hRespond "<:gregc:904127204865228851>"
  ]