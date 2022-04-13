module Main where

import BowBot.Bot
import GHC.IO.Encoding

main :: IO ()
main = do
  setLocaleEncoding utf8
  runBowBot