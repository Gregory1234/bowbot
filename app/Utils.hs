module Utils where

import Data.Time.Clock
import Data.Time.Format (defaultTimeLocale, formatTime)

setAt :: Int -> a -> [a] -> [a]
setAt i a ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 (_ : xs) = a : xs
    go n (x : xs) = x : go (n -1) xs
    go _ [] = []
{-# INLINE setAt #-}

getTime :: String -> IO String
getTime f = formatTime defaultTimeLocale f <$> getCurrentTime

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

data StatsResponse a
  = JustResponse String a
  | OldResponse String String a
  | NoResponse
  | NotOnList
  | DidYouMeanResponse String a
  | DidYouMeanOldResponse String String a