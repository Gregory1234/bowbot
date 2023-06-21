module BowBot.BotData.RefreshCommand where

import BowBot.Command
import BowBot.BotData.Download
import BowBot.Hypixel.TimeStats
import BowBot.Hypixel.Announce
import BowBot.Discord.Utils
import BowBot.Minecraft.Account
import Control.Monad.Error.Class (liftEither)
import Text.Read (readEither)
import Data.Bifunctor (first)
import qualified Data.Text as T

adminCommand :: Int -> Text -> Text -> CommandHandler () -> Command
adminCommand timeout name desc body = Command CommandInfo
  { commandName = name
  , commandHelpEntries = [HelpEntry { helpUsage = name, helpDescription = desc, helpGroup = "normal" }]
  , commandPerms = AdminLevel
  , commandTimeout = timeout
  } $ noArguments $ do
    respond "Received"
    lift body
    respond "Done"

quietAdminCommand :: Int -> Text -> Text -> CommandHandler () -> Command
quietAdminCommand timeout name desc body = Command CommandInfo
  { commandName = name
  , commandHelpEntries = [HelpEntry { helpUsage = name, helpDescription = desc, helpGroup = "normal" }]
  , commandPerms = AdminLevel
  , commandTimeout = timeout
  } $ noArguments $ lift body

updateDataCommand :: [StatsTimeRange] -> Text -> Command
updateDataCommand times name = adminCommand 3600 name ("update Bow Bot data" <> if null times then "" else " as if it was the beginning of: " <> T.intercalate ", " (map (T.toLower . statsTimeRangeName) times)) $ do
  updateBotData times
  announceMilestones

updateNamesCommand :: Command
updateNamesCommand = Command CommandInfo
  { commandName = "namesupdate"
  , commandHelpEntries = [HelpEntry { helpUsage = "namesupdate [hour]", helpDescription = "update Bow Bot Minecraft username data", helpGroup = "normal" }]
  , commandPerms = AdminLevel
  , commandTimeout = 3600
  } $ oneArgument $ \hourStr -> do
    hour <- liftEither . (first pack . readEither . unpack) $ hourStr
    respond "Received"
    updateMinecraftAccountCache hour
    respond "Done"