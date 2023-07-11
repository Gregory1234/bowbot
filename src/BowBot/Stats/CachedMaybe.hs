module BowBot.Stats.CachedMaybe where

import Data.Time (UTCTime)

data CachedMaybe a = NewJust a | CachedJust (Maybe UTCTime) a | CachedNothing deriving (Show, Eq)

isCachedNothing :: CachedMaybe a -> Bool
isCachedNothing CachedNothing = True
isCachedNothing _ = False

isAnyJust :: CachedMaybe a -> Bool
isAnyJust = not . isCachedNothing

cachedMaybe :: a -> (b -> a) -> (Maybe UTCTime -> b -> a) -> CachedMaybe b -> a
cachedMaybe n _ _ CachedNothing = n
cachedMaybe _ f _ (NewJust a) = f a
cachedMaybe _ _ f (CachedJust t a) = f t a

completeCachedMaybe :: Maybe UTCTime -> CachedMaybe a -> Maybe a -> CachedMaybe a
completeCachedMaybe time CachedNothing (Just a) = CachedJust time a
completeCachedMaybe _ c _ = c

cachedTimestamp :: Maybe UTCTime -> CachedMaybe a -> Maybe UTCTime
cachedTimestamp time (NewJust _) = time
cachedTimestamp _ (CachedJust time _) = time
cachedTimestamp _ CachedNothing = Nothing

cachedToMaybe :: CachedMaybe a -> Maybe a
cachedToMaybe = cachedMaybe Nothing Just (const Just)