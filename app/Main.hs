module Main where

import BowBot.Bot
import Data.Maybe (fromMaybe)
import System.Environment.Blank (getEnv)

main :: IO ()
main = do
  discordKey <- fromMaybe "" <$> getEnv "API_KEY"
  if discordKey /= "" then runBowBot discordKey else putStrLn "No api key provided!"