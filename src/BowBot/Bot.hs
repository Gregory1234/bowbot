{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module BowBot.Bot where

import BowBot.Utils
import BowBot.Constants
import Discord
import BowBot.Command
import BowBot.Command.Stats
import BowBot.Stats.HypixelBow
import BowBot.BotData
import BowBot.API
import Discord.Types
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (forever, when)
import Data.Foldable (for_)
import Data.Text (isPrefixOf, unpack, pack)
import Control.Monad.Reader (ReaderT(..), void, liftIO)
import Control.Concurrent (forkIO)
import System.Timeout (timeout)
import Data.Proxy (Proxy(..))
import Network.HTTP.Conduit (Manager, newManager)

runBowBot :: String -> IO ()
runBowBot discordKey = do
  ifDev () $ putStrLn "this is dev version of the bot"
  botData <- createData
  manager <- newManager managerSettings
  forever $ do
    userFacingError <-
      runDiscord $
        def
          { discordToken = pack discordKey,
            discordOnEvent = eventHandler botData manager
          }
    TIO.putStrLn userFacingError

commands :: [Command]
commands =
  [ statsCommand (Proxy @HypixelBowStats) "s" UserSettings
  , statsCommand (Proxy @HypixelBowStats) "sd" AlwaysDefault
  , statsCommand (Proxy @HypixelBowStats) "sa" AlwaysAll
  ]

eventHandler :: BotData -> Manager -> Event -> DiscordHandler ()
eventHandler bdt man (MessageCreate m) = do
  prefix <- ifDev "?" $ return "??"
  when (not (fromBot m) && prefix `isPrefixOf` messageText m) $ do
    let n = unpack $ T.toLower . T.drop (T.length prefix) . T.takeWhile (/= ' ') $ messageText m
    for_ (filter ((==n) . commandName) commands) $ \c ->
      commandTimeoutRun (commandTimeout c) $ do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        when (messageGuild m /= Just testDiscordId) $
          ifDev () $ respond m "```Attention! This is the dev version of the bot! Some features might not be avaliable! You shouldn't be reading this! If you see this message please report it immidately!```"
        commandHandler c m man bdt
      
eventHandler _ _ _ = pure ()

commandTimeoutRun :: Int -> DiscordHandler () -> DiscordHandler ()
commandTimeoutRun n x = ReaderT (void . forkIO . void . timeout (n * 1000000) . runReaderT x)

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isAdmin :: User -> Bool
isAdmin user = userId user == 422051538391793675