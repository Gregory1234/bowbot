module BowBot.Minecraft.SelectCommand where

import BowBot.Command
import BowBot.Account.Basic
import BowBot.Minecraft.Account
import BowBot.Discord.Utils
import BowBot.BotData.Cached
import BowBot.DB.Basic
import BowBot.Minecraft.Basic (uuidString)
import BowBot.Command.Utils
import Control.Monad.Except

selectMinecraftCommand :: Command
selectMinecraftCommand = Command CommandInfo
  { commandName = "selectmc"
  , commandHelpEntries = [HelpEntry { helpUsage = "selectmc [name]", helpDescription = "select one of your registered minecraft accounts as a preferred one", helpGroup = "settings" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 30
  } $ oneArgument $ \str -> do
    did <- userId <$> envs envSender
    bbacc <- liftMaybe youArentRegisteredMessage =<< getBowBotAccountByDiscord did
    mc <- liftMaybe thePlayerDoesNotExistMessage =<< getMinecraftAccountByCurrentNameFromCache str
    when (mcUUID mc `notElem` accountMinecrafts bbacc) $ throwError "*This account doesn't belong to you! If you own this account, ask an admin to add it for you.*"
    when (mcUUID mc == accountSelectedMinecraft bbacc) $ throwError "*This account is selected already!*"
    a <- (>0) <$> executeLog "INSERT INTO `peopleMinecraft` (`minecraft`, `selected`) VALUES (?,0),(?,1) ON DUPLICATE KEY UPDATE `selected`=VALUES(`selected`)" (uuidString $ accountSelectedMinecraft bbacc, uuidString $ mcUUID mc)
    if a then do
      cache <- getCache
      liftIO $ atomically $ modifyTVar cache (insertMany [(accountBotId bbacc, bbacc { accountSelectedMinecraft = mcUUID mc})])
      respond "*Selected account updated!*"
    else respond somethingWentWrongMessage
