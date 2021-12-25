{-# LANGUAGE OverloadedStrings #-}

module BowBot.API.Mojang where

import BowBot.API


mojangNameToUUID :: APIMonad m => String -> m (Maybe String)
mojangNameToUUID name = do
  let url = "https://api.mojang.com/users/profiles/minecraft/" ++ name
  res <- hSendRequestTo url url
  decodeParse res $ \o -> o .: "id"

mojangUUIDToNames :: APIMonad m => String -> m (Maybe [String])
mojangUUIDToNames uuid = do
  let url = "https://api.mojang.com/user/profiles/" ++ uuid ++ "/names"
  res <- hSendRequestTo url url
  decodeParse res $ \o -> fmap reverse . for o $ \n -> n .: "name"