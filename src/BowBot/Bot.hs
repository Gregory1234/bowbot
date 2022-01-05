{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module BowBot.Bot where

import BowBot.Utils
import Discord
import BowBot.Command
import BowBot.Command.Stats
import BowBot.Command.Register
import BowBot.Command.Simple
import BowBot.Command.Leaderboard
import BowBot.Command.Minecraft
import BowBot.Command.Watchlist
import BowBot.Command.Name
import BowBot.Command.Settings
import BowBot.Command.Snipe
import BowBot.Command.Ban
import BowBot.Command.Role
import BowBot.Command.Birthday
import BowBot.Stats.HypixelBow
import BowBot.Background
import BowBot.Snipe
import qualified Data.Text as T
import Data.Text (isPrefixOf)
import Control.Monad.Reader (ReaderT(..))
import Control.Concurrent (forkIO, forkFinally, threadDelay)
import System.Timeout (timeout)
import Network.HTTP.Conduit (newManager)
import Data.Map ((!?))
import Control.Monad (forever)
import Control.Exception.Base (SomeException, try, Exception, throw)

runBowBot :: String -> IO ()
runBowBot discordKey = do
  ifDev () $ putStrLn "this is dev version of the bot"
  botData <- createData
  manager <- newManager managerSettings
  logInfo manager "bot started"
  mkBackground botData
  forever $ do
    userFacingError <-
      runDiscord $
        def
          { discordToken = pack discordKey,
            discordOnStart = onStartup botData,
            discordOnEvent = eventHandler botData manager
          }
    logError manager $ unpack userFacingError
 where
  mkBackground bdt = void $ forkFinally (background bdt) $ \e -> do
    logError' $ show e
    mkBackground bdt
  background bdt = do
     sec <- read @Int <$> getTime "%S"
     threadDelay ((65 - sec `mod` 60) * 1000000)
     void $ forever go
   where
     go = do
       _ <- forkIO $ backgroundTimeoutRun 600 $ do
         mint <- read @Int <$> getTime "%M"
         backgroundMinutely bdt mint
       threadDelay 60000000

onStartup :: BotData -> DiscordHandler ()
onStartup bdt = do
  updateDiscordStatus
  mkBackgroundDiscord
 where
  mkBackgroundDiscord = do
    ReaderT $ \x -> void $
      forkFinally (runReaderT backgroundDiscord x) $ \e -> do
        logError' $ show e
        runReaderT mkBackgroundDiscord x
  backgroundDiscord = do
    sec <- liftIO $ read @Int <$> getTime "%S"
    liftIO $ threadDelay ((60 - sec `mod` 60) * 1000000)
    void $ forever go
    where
      go = do
        _ <- ReaderT $ \x -> forkIO $ flip runReaderT x $ backgroundDiscordTimeoutRun 600 $ do
          mint <- liftIO $ read @Int <$> getTime "%M"
          discordBackgroundMinutely bdt mint
        liftIO $ threadDelay 60000000

commands :: [Command]
commands =
  [ hypixelBowStatsCommand "s" UserSettings
  , hypixelBowStatsCommand "sd" AlwaysDefault
  , hypixelBowStatsCommand "sa" AlwaysAll
  , registerCommand "register" False True
  , urlCommand "head" True (\s -> "https://crafatar.com/avatars/" ++ s ++ "?overlay")
  , urlCommand "heada" False (\s -> "https://crafatar.com/avatars/" ++ s ++ "?overlay")
  , urlCommand "skin" True (\s -> "https://crafatar.com/renders/body/" ++ s ++ "?overlay")
  , urlCommand "skina" False (\s -> "https://crafatar.com/renders/body/" ++ s ++ "?overlay")
  , hypixelBowLeaderboardCommand "lb" "Hypixel Bow Duels Wins Leaderboard" "Wins" hypixelBowWinsLeaderboard
  , hypixelBowLeaderboardCommand "lbl" "Hypixel Bow Duels Losses Leaderboard" "Losses" hypixelBowLossesLeaderboard
  , hypixelBowLeaderboardCommand "lbs" "Hypixel Bow Duels Winstreak Leaderboard" "Winstreak" hypixelBowWinstreakLeaderboard
  , hypixelBowLeaderboardCommand "lbr" "Hypixel Bow Duels WLR Leaderboard" "WLR" hypixelBowWLRLeaderboard
  , registerCommand "add" False False
  , registerCommand "addalt" True False
  , minecraftCommand
  , listCommand
  , onlineCommand
  , nameCommand "n" True
  , nameCommand "na" False
  , settingsCommand "set" Nothing
  , settingsCommand "show" (Just (True, Always))
  , settingsCommand "hide" (Just (False, Never))
  , birthdayAnnounceCommand
  , birthdaySetCommand
  , snipeCommand
  , hypixelBowLeaderboardBanCommand "sban"
  , roleCommand
  , constStringCommand "throw" AdminLevel $ show @Integer (1 `div` 0)
  , helpCommand "help" DefaultLevel
    $ \pr -> "**Bow bot help:**\n\n"
    ++ "**Commands:**\n"
    ++ " - **" ++ pr ++ "help** - *display this message*\n"
    ++ " - **" ++ pr ++ "online** - *show all people from watchList currently in Bow Duels*\n"
    ++ " - **" ++ pr ++ "list** - *show all players in watchList*\n"
    ++ " - **" ++ pr ++ "s [name]** - *show player's Bow Duels stats*\n"
    ++ " - **" ++ pr ++ "sa [name]** - *show all Bow Duels stats*\n"
    ++ " - **" ++ pr ++ "sd [name]** - *show a default set of Bow Duels stats*\n"
    ++ " - **" ++ pr ++ "n(a) [name]** - *show player's past nicks*\n"
    ++ " - **" ++ pr ++ "head(a) [name]** - *show player's head*\n"
    ++ " - **" ++ pr ++ "skin(a) [name]** - *show player's full skin*\n"
    ++ " - **" ++ pr ++ "lb(|l|s|r) [page number|name|all]** - *show a Bow Duels leaderboard*\n"
    ++ " - **" ++ pr ++ "mc** - *list your linked minecraft nicks*\n"
    ++ " - **" ++ pr ++ "mc [name]** - *select a minecraft account as your default*\n"
    ++ " - **" ++ pr ++ "settings** - *display help for settings*\n"
    ++ " - **" ++ pr ++ "snipe** - *show the last deleted message from this channel*\n"
    ++ " - **" ++ pr ++ "role** - *show all toggleable roles*\n"
    ++ " - **" ++ pr ++ "role [name]** - *toggle a discord role*\n"
    ++ "\nMade by **GregC**#9698"
  , constStringCommand "gregc" DefaultLevel "<:gregc:904127204865228851>"
  , helpCommand "settings" DefaultLevel
    $ \pr -> "**Bow bot settings help:**\n"
    ++ "**Commands:**\n"
    ++ " - **" ++ pr ++ "settings** - *display this message*\n"
    ++ " - **" ++ pr ++ "show [stat]** - *makes the stat visible*\n"
    ++ " - **" ++ pr ++ "hide [stat]** - *makes the stat hidden*\n"
    ++ " - **" ++ pr ++ "set [stat] [yes|always|show|no|never|hide|maybe|defined]** - *sets the visibility of the stat*\n"
    ++ "*Visibility 'maybe' and 'defined' hide the stat when the value is undefined.*\n"
    ++ "**Stat names:** wins, losses, wlr, winsuntil, beststreak, currentstreak, bestdailystreak, bowhits, bowshots, accuracy\n"
    ++ "**Example:** *?show accuracy* makes accuracy visible in the ?s command\n"
  , helpCommand "modhelp" ModLevel
    $ \pr -> "**Bow bot help:**\n\n"
    ++ "**Mod Commands:**\n"
    ++ " - **" ++ pr ++ "modhelp** - *display this message*\n"
    ++ " - **" ++ pr ++ "add [discord/discord id] [name]** - *register a person with a given minecraft name*\n"
    ++ " - **" ++ pr ++ "addalt [discord/discord id] [name]** - *register a person's alt*\n"
    ++ " - **" ++ pr ++ "sban [name]** - *ban a minecraft account from the hypixel bow duels leaderboard*\n"
    ++ " - **" ++ pr ++ "bdsay** - *announce today's birthdays*\n"
    ++ " - **" ++ pr ++ "bdset [discord/discord id] [day(1-31).month(1-12)]** - *override someone's birthday*"
    ++ "\nMade by **GregC**#9698"
  ] ++ adminCommands

eventHandler :: BotData -> Manager -> Event -> DiscordHandler ()
eventHandler bdt man (MessageCreate m) = do
  liftIO $ runBotDataT (detectDeleteMessage m) bdt
  prefix <- readProp discordCommandPrefix bdt
  when (not (fromBot m) && pack prefix `isPrefixOf` messageText m) $ do
    let n = unpack $ T.toLower . T.drop (length prefix) . T.takeWhile (/= ' ') $ messageText m
    for_ (filter ((==n) . commandName) commands) $ \c ->
      withDB $ \conn ->
        commandTimeoutRun (commandTimeout c) m $ do
          logInfoDB conn $ "recieved " ++ unpack (messageText m)
          ifDev () $ do
            testDiscordId <- readProp discordGuildId bdt
            when (messageGuild m /= Just testDiscordId) $
              respond m "```Attention! This is the dev version of the bot! Some features might not be avaliable! You shouldn't be reading this! If you see this message please report it immidately!```"
          dPerms <- readProp discordPerms bdt
          let perms = fromMaybe DefaultLevel $ dPerms !? userId (messageAuthor m)
          if perms == BanLevel
          then respond m "You have been blacklisted. You can probably appeal this decision. Or not. I don't know. I'm just a pre-programmed response."
          else if perms >= commandPerms c
            then runCommandHandler (commandHandler c) m man conn bdt
            else respond m "You don't have the permission to do that!"
          logInfoDB conn $ "finished " ++ unpack (messageText m)

eventHandler bdt man (GuildMemberAdd gid mem) = do
  trueId <- readProp discordGuildId bdt
  when (gid == trueId) $
    runDiscordHandler' $ runManagerT (runBotDataT (updateDiscordRolesSingleId (userId $ memberUser mem)) bdt) man
  withDB $ \conn -> void $ executeLog conn 
      "INSERT INTO `discordDEV` (`id`, `name`, `discriminator`, `nickname`) VALUES (?,?,?,?) ON DUPLICATE KEY UPDATE `name`=VALUES(`name`), `discriminator`=VALUES(`discriminator`), `nickname`=VALUES(`nickname`)" 
      (show (userId (memberUser mem)), userName (memberUser mem), userDiscrim (memberUser mem), memberNick mem)

eventHandler _ _ _ = pure ()

timeoutDiscord :: Int -> DiscordHandler a -> DiscordHandler (Maybe a)
timeoutDiscord n x = ReaderT (timeout n . runReaderT x)

tryDiscord :: Exception e => DiscordHandler a -> DiscordHandler (Either e a)
tryDiscord x = ReaderT (try . runReaderT x)

backgroundTimeoutRun :: Int -> IO () -> IO ()
backgroundTimeoutRun n x = do
  tm <- try @SomeException (timeout (n * 1000000) x)
  case tm of
    Left e -> do
      logError' $ "Exception happened in background: " ++ show e
      throw e
    Right Nothing -> do
      logError' $ "Timed out in background: " ++ show n ++ "s"
    Right (Just ()) -> pure ()

backgroundDiscordTimeoutRun :: Int -> DiscordHandler () -> DiscordHandler ()
backgroundDiscordTimeoutRun n x = do
  tm <- tryDiscord @SomeException (timeoutDiscord (n * 1000000) x)
  case tm of
    Left e -> do
      logError' $ "Exception happened in discord background: " ++ show e
      throw e
    Right Nothing -> do
      logError' $ "Timed out in discord background: " ++ show n ++ "s"
    Right (Just ()) -> pure ()

commandTimeoutRun :: Int -> Message -> DiscordHandler () -> DiscordHandler ()
commandTimeoutRun n msg x = do
  tm <- tryDiscord @SomeException (timeoutDiscord (n * 1000000) x)
  case tm of
    Left e -> do
      logError' $ "Exception happened in command: " ++ show e
      respond msg "Something went horribly wrong! Please report this!"
      throw e
    Right Nothing -> do
      logError' $ "Timed out: " ++ show n ++ "s"
      respond msg "Timed out! Please report this!"
    Right (Just ()) -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isAdmin :: User -> Bool
isAdmin user = userId user == 422051538391793675