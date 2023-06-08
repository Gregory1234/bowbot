module BowBot.Hypixel.StatsCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Hypixel.Stats
import BowBot.Settings.Basic
import BowBot.Utils
import Discord.Types
import BowBot.Hypixel.Leaderboard
import BowBot.Hypixel.Announce
import BowBot.Discord.Roles
import BowBot.Hypixel.LeaderboardStatus
import BowBot.Minecraft.Basic
import BowBot.Discord.Utils
import Control.Monad.Except
import BowBot.Command.Utils
import BowBot.Hypixel.CommandUtils

hypixelStatsCommand :: SettingsSource -> Text -> Text -> Command
hypixelStatsCommand src name desc = Command CommandInfo
  { commandName = name
  , commandHelpEntries = [HelpEntry { helpUsage = name <> " [name]", helpDescription = desc, helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument $ \case
    Nothing -> do
      acc <- commandSelectedMinecraftByDiscordSelf
      stats <- liftMaybe thePlayerNeverJoinedHypixelMessage =<< hypixelSafeRequestStats (mcUUID acc)
      displayStats (minecraftAccountToHeader acc Nothing) stats
      updateStats (mcUUID acc) stats
    Just (uuidFromString -> Just uuid) -> do
      commandMinecraftByUUID handlerNewAccount (handlerOldAccount . autocorrectFromAccountDirect) uuid
    Just (discordIdFromString -> Just did) -> do
      acc <- commandSelectedMinecraftByDiscord did
      handlerOldAccount (autocorrectFromAccountDirect acc)
    Just n -> do
      commandMinecraftByNameWithSkipTip handlerNewAccount handlerOldAccount n
  where
    handlerOldAccount :: MinecraftAutocorrect -> ExceptT Text CommandHandler ()
    handlerOldAccount ac@MinecraftAutocorrect {autocorrectAccount = acc} = do
      stats <- liftMaybe thePlayerNeverJoinedHypixelMessage =<< hypixelSafeRequestStats (mcUUID acc)
      displayStats (minecraftAutocorrectToHeader ac) stats
      updateStats (mcUUID acc) stats
    handlerNewAccount :: MinecraftAccount -> ExceptT Text CommandHandler ()
    handlerNewAccount acc = do
      stats <- liftMaybe thePlayerNeverJoinedHypixelMessage =<< hypixelSafeRequestStats (mcUUID acc)
      let toAdd = bowWins stats >= 50
      when toAdd $ addMinecraftAccount acc
      displayStats (minecraftAccountToHeader acc Nothing) stats
      when toAdd $ updateStats (mcUUID acc) stats
    displayStats :: Text -> HypixelBowStats -> ExceptT Text CommandHandler ()
    displayStats header stats = do
      user <- envs envSender
      settings <- getSettingsFromSource src (userId user)
      respond $ header <> showHypixelBowStats settings stats
    updateStats :: UUID -> HypixelBowStats -> ExceptT Text CommandHandler ()
    updateStats uuid stats = do
      isBanned <- getHypixelIsBannedByUUID uuid
      when (isBanned == NotBanned) $ do
        void $ setHypixelBowLeaderboardEntryByUUID uuid (hypixelBowStatsToLeaderboards stats)
        applyRolesDivisionTitleByUUID uuid
        announceMilestones