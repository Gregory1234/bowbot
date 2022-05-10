{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.Hypixel.BanCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Discord.Utils
import BowBot.Minecraft.Arg
import BowBot.BotData.Cached
import BowBot.Hypixel.Leaderboard
import qualified Data.HashMap.Strict as HM
import BowBot.DB.Basic (withDB, executeLog)
import BowBot.Minecraft.Basic (uuidString)
import Database.MySQL.Simple.Types
  
hypixelBanCommand :: Command
hypixelBanCommand = Command CommandInfo
  { commandName = "sban" -- TODO: should it ban all accounts if provided a discord id?
  , commandHelpEntries = [HelpEntry { helpUsage = "sban [name]", helpDescription = "ban a player from the leaderboard", helpGroup = "normal" }]
  , commandPerms = ModLevel
  , commandTimeout = 15
  } $ oneArgument (getMinecraftAccountByCurrentNameFromCache >=> liftMaybe thePlayerDoesNotExistMessage) $ \mc -> do
    if mcHypixelBow mc == Banned
      then respond "*The player is already banned!*"
      else do
        a <- storeInCache [ mc { mcHypixelBow = Banned} ]
        b <- liftIO $ withDB $ \conn -> (>0) <$> executeLog conn "DELETE FROM `statsDEV` WHERE `minecraft` = ?" (Only (uuidString (mcUUID mc)))
        when b $ getCache @HypixelBowLeaderboardEntry >>= liftIO . atomically . flip modifyTVar (HM.delete (mcUUID mc))
        respond $ if a then "*Success, player got banned!*" else somethingWentWrongMessage