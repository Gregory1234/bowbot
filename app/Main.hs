module Main where

import BowBot.Bot
import BowBot.API
import GHC.IO.Encoding

main :: IO ()
main = do
  setLocaleEncoding utf8
  discordKey <- fromMaybe "" <$> getEnv "API_KEY"
  if discordKey /= "" then runBowBot discordKey else putStrLn "No api key provided!"