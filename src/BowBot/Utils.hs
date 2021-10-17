{-# LANGUAGE ViewPatterns #-}

module BowBot.Utils where

import Control.Monad.IO.Class ( liftIO, MonadIO )
import Data.Maybe (fromMaybe)
import System.Environment.Blank (getEnv)
import Discord.Internal.Rest (GuildId)
import BowBot.Constants
import Text.Printf (printf)
import Data.Ratio ((%))

dist :: Eq a => [a] -> [a] -> Int
dist a b =
  last
    ( if lab == 0
        then mainDiag
        else
          if lab > 0
            then lowers !! (lab - 1)
            else {- < 0 -} uppers !! (-1 - lab)
    )
  where
    mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
    uppers = eachDiag a b (mainDiag : uppers) -- upper diagonals
    lowers = eachDiag b a (mainDiag : lowers) -- lower diagonals
    eachDiag a [] diags = []
    eachDiag a (bch : bs) (lastDiag : diags) = oneDiag a bs nextDiag lastDiag : eachDiag a bs diags
      where
        nextDiag = head (tail diags)
    oneDiag a b diagAbove diagBelow = thisdiag
      where
        doDiag [] b nw n w = []
        doDiag a [] nw n w = []
        doDiag (ach : as) (bch : bs) nw n w = me : (doDiag as bs me (tail n) (tail w))
          where
            me = if ach == bch then nw else 1 + min3 (head w) nw (head n)
        firstelt = 1 + head diagBelow
        thisdiag = firstelt : doDiag a b firstelt diagAbove (tail diagBelow)
    lab = length a - length b
    min3 x y z = if x < y then x else min y z

ifDev :: MonadIO m => a -> m a -> m a
ifDev v action = do
  devmode <- liftIO $ fromMaybe "" <$> getEnv "IS_DEV"
  if devmode == "1" then action else return v
  
discordGuildId :: IO GuildId
discordGuildId = ifDev airplanesId $ return testDiscordId

discordEscape :: String -> String
discordEscape [] = ""
discordEscape (x:xs)
  | x `elem` "_*~`>" = '\\':x:discordEscape xs
  | otherwise = x:discordEscape xs      

showWLR :: Integral a => a -> a -> String
showWLR (fromIntegral -> bowWins) (fromIntegral -> bowLosses)
  | bowWins == 0, bowLosses == 0 = "NaN"
  | bowLosses == 0 = "∞"
  | otherwise = printf "%.04f" (fromRational (bowWins % bowLosses) :: Double)