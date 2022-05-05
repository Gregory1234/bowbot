{-# LANGUAGE ViewPatterns #-}

module BowBot.Utils(
  module BowBot.Utils, liftIO, MonadIO, getEnv, fromMaybe, for, for_, readMaybe,
  atomically, readTVar, writeTVar, modifyTVar, pack, unpack, when, unless, void,
  module BowBot.HoistIO
) where

import Control.Monad.IO.Class ( liftIO, MonadIO )
import Data.Maybe (fromMaybe)
import System.Environment.Blank (getEnv)
import Text.Printf (printf)
import Data.Ratio ((%))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock.POSIX (getCurrentTime)
import Data.Traversable (for)
import Data.Foldable (for_)
import Text.Read (readMaybe)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (readTVar, writeTVar, modifyTVar)
import Data.Text (pack, unpack)
import Control.Monad (when, unless, void)
import BowBot.HoistIO
import qualified Data.Map as M
import Control.Monad.Error.Class (MonadError, throwError, catchError)

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

getTime :: String -> IO String
getTime f = formatTime defaultTimeLocale f <$> getCurrentTime

getEnvOrThrow :: String -> IO String
getEnvOrThrow n = do
  v <- getEnv n
  case v of
    Nothing -> error $ n ++ " not avaliable!"
    Just s -> return s

ifDev :: MonadIO m => a -> m a -> m a
ifDev v action = do
  devmode <- liftIO $ fromMaybe "" <$> getEnv "IS_DEV"
  if devmode == "1" then action else return v

showWLR :: Integral a => a -> a -> String
showWLR (fromIntegral -> bowWins) (fromIntegral -> bowLosses)
  | bowWins == 0, bowLosses == 0 = "NaN"
  | bowLosses == 0 = "∞"
  | otherwise = printf "%.04f" (fromRational (bowWins % bowLosses) :: Double)

liftMaybe :: MonadError e m => e -> Maybe a -> m a
liftMaybe _ (Just a) = return a
liftMaybe e Nothing = throwError e

assertIO :: MonadIO m => Bool -> m ()
assertIO x = liftIO $ do
  True <- return x
  pure ()

groupByToMap :: Ord k => (v -> k) -> [v] -> M.Map k [v]
groupByToMap _ [] = M.empty
groupByToMap f (x:xs) = M.insertWith (++) (f x) [x] $ groupByToMap f xs

pad' :: Bool -> Char -> Int -> String -> String
pad' d c l x = if d then x ++ replicate (l - length x) c else replicate (l - length x) c ++ x

pad :: Int -> String -> String
pad = pad' True ' '

catchErrorEither :: MonadError e m => m a -> m (Either e a)
catchErrorEither body = catchError (Right <$> body) (return . Left)