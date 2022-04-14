{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}

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
import BowBot.Discord.Class (MonadDiscord, call_)
import Control.Exception.Base (SomeException, try, throw)
import System.Timeout (timeout)
import Control.Monad.Reader (ReaderT(..))
import BowBot.BotData.Download
import BowBot.BotData.RefreshCommand
import Data.Proxy
import BowBot.BotData.Cached

runBowBot :: IO ()
runBowBot = do
  discordKey <- getEnvOrThrow "API_KEY"
  ifDev () $ putStrLn "this is dev version of the bot"
  manager <- newManager managerSettings
  bdt <- downloadBotData
  logInfo "bot started"
  forever $ do
    userFacingError <-
      runDiscord $
        def
          { discordToken = pack discordKey,
            discordOnStart = ReaderT $ runBotT onStartup bdt manager,
            discordOnEvent = \e -> ReaderT $ runBotT (eventHandler e) bdt manager,
            discordOnLog = putStrLn . unpack
          }
    logError $ unpack userFacingError

onStartup :: Bot ()
onStartup = do
  pure ()

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
      for_ (filter ((==n) . commandName . anyCommandInfo) commands) $ \c ->
        commandTimeoutRun (commandTimeout $ anyCommandInfo c) m $ do
          logInfo $ "recieved " ++ unpack (messageContent m)
          ifDev () $ do
            testDiscordId <- hInfoDB discordGuildIdInfo
            when (messageGuildId m /= Just testDiscordId) $
              respond m "```Attention! This is the dev version of the bot! Some features might not be avaliable! You shouldn't be reading this! If you see this message please report it immidately!```"
          perms <- fromMaybe DefaultLevel <$> getFromCache (Proxy @PermissionLevel) (userId (messageAuthor m))
          if perms == BanLevel
          then respond m "You have been blacklisted. You can probably appeal this decision. Or not. I don't know. I'm just a pre-programmed response."
          else if perms >= commandPerms (anyCommandInfo c)
            then runAnyCommand c m
            else respond m "You don't have the permission to do that!"
          logInfo $ "finished " ++ unpack (messageContent m)
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

commands :: [AnyCommand]
commands =
  [ AnyCommand hypixelStatsCommand
  , AnyCommand refreshDataCommand
  ]