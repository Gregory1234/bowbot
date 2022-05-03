{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.Hypixel.BanCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Discord.Utils
import BowBot.Minecraft.Arg
import Data.Proxy
import BowBot.BotData.Cached
import Control.Monad (join)
import BowBot.Hypixel.Leaderboard
import qualified Data.HashMap.Strict as HM
import BowBot.DB.Basic (withDB, executeLog)
import BowBot.Minecraft.Basic (uuidString)
import Database.MySQL.Simple.Types
  
hypixelBanCommand :: Command
hypixelBanCommand = Command CommandInfo
  { commandName = "sban"
  , commandUsage = "sban [name]"
  , commandDescription = "ban a player from the leaderboard" -- TODO: should it ban all accounts if provided a discord id?
  , commandPerms = ModLevel
  , commandTimeout = 15
  , commandGroup = "normal" -- TODO: don't use mcNameToUUID unnecessarily
  } $ hOneArgument (\n -> mcNameToUUID n >>= traverse (getFromCache (Proxy @MinecraftAccount)) >>= liftMaybe thePlayerDoesNotExistMessage . join) $ \mc -> do
    if mcHypixelBow mc == Banned
      then hRespond "*The player is already banned!*"
      else do
        a <- storeInCache [ mc { mcHypixelBow = Banned} ]
        b <- liftIO $ withDB $ \conn -> (>0) <$> executeLog conn "DELETE FROM `statsDEV` WHERE `minecraft` = ?" (Only (uuidString (mcUUID mc)))
        when b $ getCache (Proxy @HypixelBowLeaderboardEntry) >>= liftIO . atomically . flip modifyTVar (HM.delete (mcUUID mc))
        hRespond $ if a then "*Success, player got banned!*" else somethingWentWrongMessage