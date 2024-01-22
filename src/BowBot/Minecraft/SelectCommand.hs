{-# LANGUAGE QuasiQuotes #-}

module BowBot.Minecraft.SelectCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Discord.Utils
import BowBot.DB.Basic
import BowBot.Command.Utils
import Control.Monad.Except
import BowBot.Account.Basic
import BowBot.Minecraft.Basic

selectMinecraftCommand :: Command
selectMinecraftCommand = Command CommandInfo
  { commandName = "selectmc"
  , commandHelpEntries = [HelpEntry { helpUsage = "selectmc [name]", helpDescription = "select one of your registered minecraft accounts as a preferred one", helpGroup = "settings" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 30
  } $ oneArgument $ \str -> do
    did <- userId <$> envs envSender
    mcs <- liftMaybe youArentRegisteredMessage =<< getMinecraftListByDiscord did
    mc <- liftMaybe thePlayerDoesNotExistMessage =<< getMinecraftAccountByCurrentName str
    when (mcUUID mc `notElem` allMinecrafts mcs) $ throwError "*This account doesn't belong to you! If you own this account, ask an admin to add it for you.*"
    when (mcUUID mc == selectedMinecraft mcs) $ throwError "*This account is selected already!*"
    let selectedUUID = selectedMinecraft mcs; newUUID = mcUUID mc
    a <- (>0) <$> executeLog [mysql|INSERT INTO `account_minecraft` (`minecraft_uuid`, ^`selected`) VALUES (selectedUUID,0),(newUUID,1)|]
    respond $ if a then "*Selected account updated!*" else somethingWentWrongMessage
