module Main where

import BowBot.Bot
import BowBot.API

main :: IO ()
main = do
  discordKey <- fromMaybe "" <$> getEnv "API_KEY"
  if discordKey /= "" then runBowBot discordKey else putStrLn "No api key provided!"