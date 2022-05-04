{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.Minecraft.SelectCommand where

import BowBot.Command
import BowBot.Minecraft.Arg
import BowBot.Account.Basic
import BowBot.Minecraft.Account
import Control.Monad ((>=>))
import BowBot.Discord.Utils
import BowBot.BotData.Cached
import Data.Proxy
import BowBot.DB.Basic
import BowBot.Minecraft.Basic (uuidString)


selectMinecraftCommand :: Command
selectMinecraftCommand = Command CommandInfo
  { commandName = "selectmc"
  , commandHelpEntries = [HelpEntry { helpUsage = "selectmc [name]", helpDescription = "select one of your registered minecraft accounts as a preferred one", helpGroup = "settings" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 30
  } $ hOneArgument (getMinecraftAccountByCurrentNameFromCache >=> liftMaybe "*This account doesn't exist!*") $ \mc -> do
    acc' <- getBowBotAccountByDiscord . userId =<< hEnv envSender
    case acc' of
      Nothing -> hRespond youArentRegisteredMessage
      Just acc -> do
        if mcUUID mc `notElem` accountMinecrafts acc
          then hRespond "*This account doesn't belong to you!*"
          else if mcUUID mc == accountSelectedMinecraft acc
            then hRespond "*This account is selected already!*"
            else do
              a <- liftIO $ withDB $ \conn -> (>0) <$> executeLog conn "INSERT INTO `peopleMinecraftDEV` (`minecraft`, `selected`) VALUES (?,0),(?,1) ON DUPLICATE KEY UPDATE `selected`=VALUES(`selected`)" (uuidString $ accountSelectedMinecraft acc, uuidString $ mcUUID mc)
              if a then do
                cache <- getCache (Proxy @BowBotAccount)
                liftIO $ atomically $ modifyTVar cache (insertMany [(BowBot.Account.Basic.accountId acc, acc { accountSelectedMinecraft = mcUUID mc})])
                hRespond "*Selected account updated!*"
              else hRespond somethingWentWrongMessage
