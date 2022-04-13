{-# LANGUAGE OverloadedStrings #-}

module BowBot.Minecraft.Basic where

import BowBot.Network.Class
import Data.Aeson
import BowBot.Network.Basic (decodeParse)
import Data.Traversable (for)
import BowBot.DB.Class

newtype UUID = UUID { uuidString :: String } deriving (Show, Eq, Ord)
  
mojangNameToUUID :: (MonadNetwork m, MonadDB m) => String -> m (Maybe UUID)
mojangNameToUUID name = do
  let url = "https://api.mojang.com/users/profiles/minecraft/" ++ name
  res <- hSendRequestTo url url
  decodeParse res $ \o -> UUID <$> o .: "id"

mojangUUIDToNames :: (MonadNetwork m, MonadDB m) => UUID -> m (Maybe [String])
mojangUUIDToNames (UUID uuid) = do
  let url = "https://api.mojang.com/user/profiles/" ++ uuid ++ "/names"
  res <- hSendRequestTo url url
  decodeParse res $ \o -> fmap reverse . for o $ \n -> n .: "name"