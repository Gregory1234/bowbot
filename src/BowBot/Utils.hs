{-# LANGUAGE TypeFamilies #-}

module BowBot.Utils(
  module BowBot.Utils, module BowBot.HoistIO, getEnv, for, for_, readMaybe, (<|>), ($>), foldl',
  STM, atomically, TVar, newTVar, readTVar, writeTVar, modifyTVar, pack, unpack, Has(..), Text, TextShow(..),
  module Data.Char, module Data.List, module Data.List.Split, module Data.Maybe, module Control.Monad.Reader
) where

import Data.Maybe
import System.Environment.Blank (getEnv)
import Text.Printf (printf)
import Data.Ratio ((%))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock.POSIX (getCurrentTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Traversable (for)
import Data.Foldable (for_, foldl')
import Text.Read (readMaybe)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar, modifyTVar)
import Data.Text (pack, unpack, Text)
import qualified Data.Text as T
import BowBot.HoistIO
import qualified Data.Map as M
import Control.Monad.Error.Class (MonadError, throwError, catchError)
import Control.Monad.Reader
import Control.Applicative ((<|>))
import Data.Char
import Data.List (sortOn, intercalate, intersperse, isPrefixOf, isSuffixOf, (\\), intersect, find, findIndex)
import Data.Functor (($>))
import Data.List.Split (splitOn, chunksOf)
import Data.Kind (Type, Constraint)
import Data.Has
import Data.Time.Clock (UTCTime(..), nominalDiffTimeToSeconds)
import Data.Fixed (Fixed(..), resolution)
import TextShow
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Except (ExceptT, runExceptT)

dist :: Text -> Text -> Int
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
    eachDiag a "" diags = []
    eachDiag a (T.uncons -> Just (bch, bs)) (lastDiag : diags) = oneDiag a bs nextDiag lastDiag : eachDiag a bs diags
      where
        nextDiag = head (tail diags)
    oneDiag a b diagAbove diagBelow = thisdiag
      where
        doDiag "" b nw n w = []
        doDiag a "" nw n w = []
        doDiag (T.uncons -> Just (ach, as)) (T.uncons -> Just (bch, bs)) nw n w = me : (doDiag as bs me (tail n) (tail w))
          where
            me = if ach == bch then nw else 1 + min3 (head w) nw (head n)
        firstelt = 1 + head diagBelow
        thisdiag = firstelt : doDiag a b firstelt diagAbove (tail diagBelow)
    lab = T.length a - T.length b
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

data WLR a = WLR { wlrWins :: a, wlrLosses :: a } deriving (Show, Eq)

instance (Num a, Ord a) => Ord (WLR a) where
  (WLR w1 l1) <= (WLR w2 l2) = if w1 * l2 == w2 * l1 then w1 <= w2 else w1 * l2 <= w2 * l1

showWLR :: Integral a => WLR a -> Text
showWLR (WLR (fromIntegral -> bowWins) (fromIntegral -> bowLosses))
  | bowWins == 0, bowLosses == 0 = "NaN"
  | bowLosses == 0 = "∞"
  | otherwise = pack $ printf "%.04f" (fromRational (bowWins % bowLosses) :: Double)

liftMaybe :: MonadError e m => e -> Maybe a -> m a
liftMaybe _ (Just a) = return a
liftMaybe e Nothing = throwError e

runMaybeT :: Functor m => ExceptT e m a -> m (Maybe a)
runMaybeT = fmap (either (const Nothing) Just) . runExceptT

assertIO :: MonadIO m => Bool -> m ()
assertIO x = liftIO $ do
  True <- return x
  pure ()

groupByToMap :: Ord k => (v -> k) -> [v] -> M.Map k [v]
groupByToMap _ [] = M.empty
groupByToMap f (x:xs) = M.insertWith (++) (f x) [x] $ groupByToMap f xs

pad' :: Bool -> Char -> Int -> Text -> Text
pad' d c l x = if d then x <> T.replicate (l - T.length x) (T.singleton c) else T.replicate (l - T.length x) (T.singleton c) <> x

pad :: Int -> Text -> Text
pad = pad' True ' '

padbs' :: Bool -> Char -> Int -> ByteString -> ByteString
padbs' d c l x = if d then x <> BS.replicate (l - BS.length x) c else BS.replicate (l - BS.length x) c <> x

padbs :: Int -> Text -> Text
padbs = pad' True ' '

catchErrorEither :: MonadError e m => m a -> m (Either e a)
catchErrorEither body = catchError (Right <$> body) (return . Left)

discordEscape :: Text -> Text
discordEscape = helper ("_*~`>" :: String)
  where
    helper (c:cs) = helper cs . T.replace (T.singleton c) (T.pack ['\\', c])
    helper [] = id

type MonadIOReader m r = (MonadIO m, MonadReader r m)
type MonadHoistIOReader m r = (MonadHoistIO m, MonadReader r m)

type family HasAll (as :: [Type]) r :: Constraint where
  HasAll '[] r = ()
  HasAll (a ': as) r = (Has a r, HasAll as r)

orElseError :: (MonadError e m) => m a -> m a -> m a
orElseError a b = catchError a $ const b

timestampFromUnixSecond :: Integer -> UTCTime
timestampFromUnixSecond = posixSecondsToUTCTime . fromInteger

timestampToUnixSecond :: UTCTime -> Integer
timestampToUnixSecond timestamp = fullseconds
  where
    seconds = nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds timestamp
    fullseconds = let (MkFixed picoseconds) = seconds in picoseconds `div` resolution seconds

zeroTimestamp :: UTCTime
zeroTimestamp = UTCTime { utctDay = toEnum (-678941), utctDayTime = 0 }

nullZeroTime :: UTCTime -> Maybe UTCTime
nullZeroTime x
  | x == zeroTimestamp = Nothing
  | otherwise = Just x

unNullZeroTime :: Maybe UTCTime -> UTCTime
unNullZeroTime = fromMaybe zeroTimestamp

only :: [a] -> Maybe a
only [a] = Just a
only _ = Nothing

filterMaybe :: (a -> Bool) -> a -> Maybe a
filterMaybe f a = if f a then Just a else Nothing