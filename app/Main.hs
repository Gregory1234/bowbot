{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Data.Text (Text, isPrefixOf, toLower, pack)
import qualified Data.Text.IO as TIO
import Discord
import qualified Discord.Requests as R
import Discord.Types
import System.Environment.Blank (getEnv)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  apiKey <- fromMaybe "" <$> getEnv "API_KEY"
  userFacingError <-
    runDiscord $
      def
        { discordToken = pack apiKey,
          discordOnEvent = eventHandler
        }
  TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
  MessageCreate m -> do
    command m "?s" $ do
      _ <- restCall (R.CreateMessage (messageChannel m) "Pong!")
      pure ()
  _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

command :: Applicative m => Message -> Text -> m () -> m ()
command m c = when (not (fromBot m) && isCommand c (messageText m))

isCommand :: Text -> Text -> Bool
isCommand c = (c `isPrefixOf`) . toLower
