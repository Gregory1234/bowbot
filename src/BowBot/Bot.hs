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
import BowBot.Stats
import BowBot.Stats.HypixelBow
import BowBot.API
import BowBot.Background
import BowBot.Snipe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (isPrefixOf)
import Control.Monad.Reader (ReaderT(..))
import Control.Concurrent (forkIO, forkFinally, threadDelay)
import System.Timeout (timeout)
import Network.HTTP.Conduit (newManager)
import Data.Map ((!?))
import Control.Monad (forever)

runBowBot :: String -> IO ()
runBowBot discordKey = do
  ifDev () $ putStrLn "this is dev version of the bot"
  botData <- createData
  manager <- newManager managerSettings
  mkBackground botData
  forever $ do
    userFacingError <-
      runDiscord $
        def
          { discordToken = pack discordKey,
            discordOnStart = onStartup botData,
            discordOnEvent = eventHandler botData manager
          }
    TIO.putStrLn userFacingError
 where
  mkBackground bdt = void $ forkFinally (background bdt) $ \e -> do
    print e
    mkBackground bdt
  background bdt = do
     sec <- read @Int <$> getTime "%S"
     threadDelay ((65 - sec `mod` 60) * 1000000)
     void $ forever go
   where
     go = do
       _ <- forkIO $ do
         mint <- read @Int <$> getTime "%M"
         putStrLn "New minute!"
         backgroundMinutely bdt mint
         putStrLn "New minute finished!"
       threadDelay 60000000

onStartup :: BotData -> DiscordHandler ()
onStartup bdt = do
  sendCommand (UpdateStatus $ UpdateStatusOpts {
    updateStatusOptsSince = Nothing,
    updateStatusOptsGame = Just (Activity {activityName = "NEW UPDATE NOW!", activityType = ActivityTypeGame, activityUrl = Nothing}), -- TODO: make this controllable from db
    updateStatusOptsNewStatus = UpdateStatusOnline,
    updateStatusOptsAFK = False
  })
  mkBackgroundDiscord
 where
  mkBackgroundDiscord = do
    ReaderT $ \x -> void $
      forkFinally (runReaderT backgroundDiscord x) $ \e -> do
        print e
        runReaderT mkBackgroundDiscord x
  backgroundDiscord = do
    sec <- liftIO $ read @Int <$> getTime "%S"
    liftIO $ threadDelay ((60 - sec `mod` 60) * 1000000)
    void $ forever go
    where
      go = do
        _ <- ReaderT $ \x -> forkIO $ flip runReaderT x $ do
          mint <- liftIO $ read @Int <$> getTime "%M"
          liftIO $ putStrLn "New discord minute!"
          discordBackgroundMinutely bdt mint
          liftIO $ putStrLn "New discord minute finished!"
        liftIO $ threadDelay 60000000

commands :: [Command]
commands =
  [ statsCommand (Proxy @HypixelBowStats) "s" hypixelRequestCounter UserSettings
  , statsCommand (Proxy @HypixelBowStats) "sd" hypixelRequestCounter AlwaysDefault
  , statsCommand (Proxy @HypixelBowStats) "sa" hypixelRequestCounter AlwaysAll
  , registerCommand "register" [hypixelRequestCounter] False True $ \man uuid -> do
      fullUpdateStats (Proxy @HypixelBowStats) man uuid
  , urlCommand "head" True (\s -> "https://crafatar.com/avatars/" ++ s ++ "?overlay")
  , urlCommand "heada" False (\s -> "https://crafatar.com/avatars/" ++ s ++ "?overlay")
  , urlCommand "skin" True (\s -> "https://crafatar.com/renders/body/" ++ s ++ "?overlay")
  , urlCommand "skina" False (\s -> "https://crafatar.com/renders/body/" ++ s ++ "?overlay")
  , leaderboardCommand (Proxy @HypixelBowStats) "lb" "Hypixel Bow Duels Wins Leaderboard" "Wins" hypixelBowWinsLeaderboard
  , leaderboardCommand (Proxy @HypixelBowStats) "lbl" "Hypixel Bow Duels Losses Leaderboard" "Losses" hypixelBowLossesLeaderboard
  , leaderboardCommand (Proxy @HypixelBowStats) "lbs" "Hypixel Bow Duels Winstreak Leaderboard" "Winstreak" hypixelBowWinstreakLeaderboard
  , leaderboardCommand (Proxy @HypixelBowStats) "lbr" "Hypixel Bow Duels WLR Leaderboard" "WLR" hypixelBowWLRLeaderboard
  , registerCommand "add" [hypixelRequestCounter] False False $ \man uuid -> do
      fullUpdateStats (Proxy @HypixelBowStats) man uuid
  , registerCommand "addalt" [hypixelRequestCounter] True False $ \man uuid -> do
      fullUpdateStats (Proxy @HypixelBowStats) man uuid
  , minecraftCommand
  , listCommand
  , onlineCommand
  , nameCommand "n" True
  , nameCommand "na" False
  , settingsCommand "set" Nothing
  , settingsCommand "show" (Just (True, Always))
  , settingsCommand "hide" (Just (False, Never))
  , snipeCommand
  , constStringCommand "help" DefaultLevel
    $ "**Bow bot help:**\n\n"
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
    ++ " - **?lb(|l|s|r) [page number|name|all]** - *show a Bow Duels leaderboard*\n"
    ++ " - **?mc** - *list your linked minecraft nicks*\n"
    ++ " - **?mc [name]** - *select a minecraft account as your default*\n"
    ++ " - **?settings** - *display help for settings*\n"
    ++ " - **?snipe** - *show the last deleted message from this channel*\n"
    ++ "\nMade by **GregC**#9698"
  , constStringCommand "settings" DefaultLevel
    $ "**Bow bot settings help:**\n"
    ++ "**Commands:**\n"
    ++ " - **?settings** - *display this message*\n"
    ++ " - **?show [stat]** - *makes the stat visible*\n"
    ++ " - **?hide [stat]** - *makes the stat hidden*\n"
    ++ " - **?set [stat] [yes|always|show|no|never|hide|maybe|defined]** - *sets the visibility of the stat*\n"
    ++ "*Visibility 'maybe' and 'defined' hide the stat when the value is undefined.*\n"
    ++ "**Stat names:** wins, losses, wlr, winsuntil, beststreak, currentstreak, bestdailystreak, bowhits, bowshots, accuracy\n"
    ++ "**Example:** *?show accuracy* makes accuracy visible in the ?s command\n"
  , constStringCommand "modhelp" ModLevel
    $ "**Bow bot help:**\n\n"
    ++ "**Mod Commands:**\n"
    ++ " - **?modhelp** - *display this message*\n"
    ++ " - **?add [discord/discord id] [name]** - *register a person with a given minecraft name*\n"
    ++ " - **?addalt [discord/discord id] [name]** - *register a person's alt*\n"
    ++ "\nMade by **GregC**#9698"
  ] ++ adminCommands

eventHandler :: BotData -> Manager -> Event -> DiscordHandler ()
eventHandler bdt man (MessageCreate m) = do
  liftIO $ atomically $ detectDeleteMessage bdt m
  prefix <- ifDev "?" $ return "??"
  when (not (fromBot m) && prefix `isPrefixOf` messageText m) $ do
    let n = unpack $ T.toLower . T.drop (T.length prefix) . T.takeWhile (/= ' ') $ messageText m
    for_ (filter ((==n) . commandName) commands) $ \c ->
      commandTimeoutRun (commandTimeout c) $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        when (messageGuild m /= Just testDiscordId) $
          ifDev () $ respond m "```Attention! This is the dev version of the bot! Some features might not be avaliable! You shouldn't be reading this! If you see this message please report it immidately!```"
        dPerms <- liftIO $ atomically $ readTVar $ discordPerms bdt
        let perms = fromMaybe DefaultLevel $ dPerms !? userId (messageAuthor m)
        if perms == BanLevel
        then respond m "You have been blacklisted. You can probably appeal this decision. Or not. I don't know. I'm just a pre-programmed response."
        else if perms >= commandPerms c
          then commandHandler c m man bdt
          else respond m "You don't have the permission to do that!"
      
eventHandler _ _ _ = pure ()

commandTimeoutRun :: Int -> DiscordHandler () -> DiscordHandler ()
commandTimeoutRun n x = ReaderT (void . forkIO . void . timeout (n * 1000000) . runReaderT x)

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isAdmin :: User -> Bool
isAdmin user = userId user == 422051538391793675