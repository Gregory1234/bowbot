{-# LANGUAGE OverloadedStrings #-}

module BowBot.Minecraft.SelectCommand where

import BowBot.Command
import BowBot.Minecraft.Arg
import BowBot.Account.Basic
import BowBot.Minecraft.Account
import Control.Monad ((>=>))
import BowBot.Discord.Utils
import BowBot.BotData.Cached
import BowBot.DB.Basic
import BowBot.Minecraft.Basic (uuidString)


selectMinecraftCommand :: Command
selectMinecraftCommand = Command CommandInfo
  { commandName = "selectmc"
  , commandHelpEntries = [HelpEntry { helpUsage = "selectmc [name]", helpDescription = "select one of your registered minecraft accounts as a preferred one", helpGroup = "settings" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 30
  } $ oneArgument (getMinecraftAccountByCurrentNameFromCache >=> liftMaybe "*This account doesn't exist!*") $ \mc -> do
    acc' <- getBowBotAccountByDiscord . userId =<< envs envSender
    case acc' of
      Nothing -> respond youArentRegisteredMessage
      Just acc -> do
        if mcUUID mc `notElem` accountMinecrafts acc
          then respond "*This account doesn't belong to you!*"
          else if mcUUID mc == accountSelectedMinecraft acc
            then respond "*This account is selected already!*"
            else do
              a <- liftIO $ withDB $ \conn -> (>0) <$> executeLog conn "INSERT INTO `peopleMinecraftDEV` (`minecraft`, `selected`) VALUES (?,0),(?,1) ON DUPLICATE KEY UPDATE `selected`=VALUES(`selected`)" (uuidString $ accountSelectedMinecraft acc, uuidString $ mcUUID mc)
              if a then do
                cache <- getCache
                liftIO $ atomically $ modifyTVar cache (insertMany [(BowBot.Account.Basic.accountId acc, acc { accountSelectedMinecraft = mcUUID mc})])
                respond "*Selected account updated!*"
              else respond somethingWentWrongMessage
