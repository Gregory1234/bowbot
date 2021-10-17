{-# LANGUAGE OverloadedStrings #-}

module BowBot.API.Mojang where

import Data.Aeson.Types (parseMaybe, (.:))
import Data.Aeson (decode)
import BowBot.API (sendRequestTo)
import Network.HTTP.Conduit (Manager)
import Data.Traversable (for)


mojangNameToUUID :: Manager -> String -> IO (Maybe String)
mojangNameToUUID manager name = do
  let url = "https://api.mojang.com/users/profiles/minecraft/" ++ name
  res <- sendRequestTo manager url url
  let parser = parseMaybe $ \o -> o .: "id"
  return $ decode res >>= parser

mojangUUIDToNames :: Manager -> String -> IO (Maybe [String])
mojangUUIDToNames manager uuid = do
  let url = "https://api.mojang.com/user/profiles/" ++ uuid ++ "/names"
  res <- sendRequestTo manager url url
  let parser = parseMaybe $ \o -> for o $ \n -> n .: "name"
  return $ decode res >>= parser . reverse