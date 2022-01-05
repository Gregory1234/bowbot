{-# LANGUAGE OverloadedStrings #-}

module BowBot.API.Mojang where

import BowBot.API


mojangNameToUUID :: APIMonad m => String -> m (Maybe UUID)
mojangNameToUUID name = do
  let url = "https://api.mojang.com/users/profiles/minecraft/" ++ name
  res <- hSendRequestTo url url
  decodeParse res $ \o -> UUID <$> o .: "id"

mojangUUIDToNames :: APIMonad m => UUID -> m (Maybe [String])
mojangUUIDToNames (UUID uuid) = do
  let url = "https://api.mojang.com/user/profiles/" ++ uuid ++ "/names"
  res <- hSendRequestTo url url
  decodeParse res $ \o -> fmap reverse . for o $ \n -> n .: "name"