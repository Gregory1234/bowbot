{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.Minecraft.Basic where

import BowBot.Network.Basic
import Data.Hashable (Hashable)
import BowBot.Utils (MonadIOReader, toLower)
import Data.Foldable (toList)
import Text.Regex
import Text.Regex.Base
import qualified Data.ByteString.Lazy.Char8 as B

newtype UUID = UUID { uuidString :: String }
  deriving (Show, Eq, Ord)
  deriving newtype (Hashable)

uuidFromString :: String -> Maybe UUID
uuidFromString str
  | length str == 36
  , str !! 8 == '-'
  , str !! 13 == '-'
  , str !! 18 == '-'
  , str !! 32 == '-'
  = uuidFromString $ filter (/='-') str
  | length str == 32
  , all (`elem` ("1234567890abcdefABCDEF" :: String)) str
  = Just . UUID $ map toLower str
  | otherwise = Nothing

mojangNameToUUID :: (MonadIOReader m r, Has Manager r) => String -> m (Maybe UUID)
mojangNameToUUID name = do
  let url = "https://api.mojang.com/users/profiles/minecraft/" ++ name
  res <- sendRequestTo url url
  decodeParse res $ \o -> UUID <$> o .: "id"

mojangUUIDToCurrentName :: (MonadIOReader m r, Has Manager r) => UUID -> m (Maybe String)
mojangUUIDToCurrentName (UUID uuid) = do
  let url = "https://sessionserver.mojang.com/session/minecraft/profile/" ++ uuid
  res <- sendRequestTo url url
  decodeParse res $ \o -> o .: "name"