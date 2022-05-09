{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.Minecraft.Basic where

import BowBot.Network.Basic
import Data.Aeson
import Data.Traversable (for)
import Data.Hashable (Hashable)

newtype UUID = UUID { uuidString :: String }
  deriving (Show, Eq, Ord)
  deriving newtype (Hashable)
  
mojangNameToUUID :: (MonadIO m, MonadReader r m, Has Manager r) => String -> m (Maybe UUID)
mojangNameToUUID name = do
  let url = "https://api.mojang.com/users/profiles/minecraft/" ++ name
  res <- sendRequestTo url url
  decodeParse res $ \o -> UUID <$> o .: "id"

mojangUUIDToNames :: (MonadIO m, MonadReader r m, Has Manager r) => UUID -> m (Maybe [String])
mojangUUIDToNames (UUID uuid) = do
  let url = "https://api.mojang.com/user/profiles/" ++ uuid ++ "/names"
  res <- sendRequestTo url url
  decodeParse res $ \o -> fmap reverse . for o $ \n -> n .: "name"