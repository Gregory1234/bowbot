{-# LANGUAGE OverloadedStrings #-}

module BowBot.API.Mojang where

import Data.Aeson.Types ((.:))
import BowBot.API
import Network.HTTP.Conduit (Manager)
import Data.Traversable (for)


mojangNameToUUID :: Manager -> String -> IO (Maybe String)
mojangNameToUUID manager name = do
  let url = "https://api.mojang.com/users/profiles/minecraft/" ++ name
  res <- sendRequestTo manager url url
  decodeParse res $ \o -> o .: "id"

mojangUUIDToNames :: Manager -> String -> IO (Maybe [String])
mojangUUIDToNames manager uuid = do
  let url = "https://api.mojang.com/user/profiles/" ++ uuid ++ "/names"
  res <- sendRequestTo manager url url
  decodeParse res $ \o -> for o $ \n -> n .: "name"