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
import BowBot.Hypixel.TimeStats
import BowBot.Account.Utils

data StatsCommandSettings a = StatsCommandSettings
  { requestStats :: Bool -> UUID -> ExceptT Text CommandHandler a
  , shouldAddAccount :: a -> Bool
  , updateStats :: UUID -> a -> ExceptT Text CommandHandler ()
  , showStats :: Settings -> a -> Text
  }

statsCommandTemplate :: forall a. StatsCommandSettings a -> SettingsSource -> Text -> Text -> Command
statsCommandTemplate StatsCommandSettings {..} src name desc = Command CommandInfo
  { commandName = name
  , commandHelpEntries = [HelpEntry { helpUsage = name <> " [name]", helpDescription = desc, helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument $ \case
    Nothing -> do
      did <- userId <$> envs envSender
      acc <- liftMaybe youArentRegisteredMessage =<< getSelectedMinecraftByDiscord did
      stats <- requestStats True (mcUUID acc)
      displayStats (minecraftAccountToHeader acc Nothing) stats
      updateStatsUnlessBanned (mcUUID acc) stats
    Just (uuidFromString -> Just uuid) -> do
      stats <- requestStats False uuid
      commandMinecraftByUUID (handlerNewAccount stats) (handlerOldAccount stats . autocorrectFromAccountDirect) uuid
    Just (discordIdFromString -> Just did) -> do
      acc <- liftMaybe theUserIsntRegisteredMessage =<< getSelectedMinecraftByDiscord did
      stats <- requestStats False (mcUUID acc)
      handlerOldAccount stats (autocorrectFromAccountDirect acc)
    Just n -> do
      commandMinecraftByNameWithExtraWithSkipTip (requestStats False) handlerNewAccount handlerOldAccount n
  where
    handlerOldAccount :: a -> MinecraftAutocorrect -> ExceptT Text CommandHandler ()
    handlerOldAccount stats ac@MinecraftAutocorrect {autocorrectAccount = acc} = do
      displayStats (minecraftAutocorrectToHeader ac) stats
      updateStatsUnlessBanned (mcUUID acc) stats
    handlerNewAccount :: a -> MinecraftAccount -> ExceptT Text CommandHandler ()
    handlerNewAccount stats acc = do
      let toAdd = shouldAddAccount stats
      when toAdd $ addMinecraftAccount acc
      displayStats (minecraftAccountToHeader acc Nothing) stats
      when toAdd $ updateStats (mcUUID acc) stats
    displayStats :: Text -> a -> ExceptT Text CommandHandler ()
    displayStats header stats = do
      user <- envs envSender
      settings <- getSettingsFromSource src (userId user)
      respond $ header <> showStats settings stats
    updateStatsUnlessBanned :: UUID -> a -> ExceptT Text CommandHandler ()
    updateStatsUnlessBanned uuid stats = do
      isBanned <- getHypixelIsBannedByUUID uuid
      when (isBanned == NotBanned) $ updateStats uuid stats

hypixelStatsCommand :: SettingsSource -> Text -> Text -> Command
hypixelStatsCommand = statsCommandTemplate StatsCommandSettings
  { requestStats = \b -> liftMaybe (if b then youNeverJoinedHypixelMessage else thePlayerNeverJoinedHypixelMessage) <=< hypixelSafeRequestStats
  , shouldAddAccount = (>= 50) . bowWins
  , updateStats = \uuid stats -> do
      void $ setHypixelBowLeaderboardEntryByUUID uuid (hypixelBowStatsToLeaderboards stats)
      applyRolesDivisionTitleByUUID uuid
      announceMilestones
  , showStats = showHypixelBowStats
  }

hypixelTimeStatsCommand :: SettingsSource -> Text -> Text -> Command
hypixelTimeStatsCommand = statsCommandTemplate StatsCommandSettings
  { requestStats = \b uuid -> do
    currentHypixelBowStats <- liftMaybe (if b then youNeverJoinedHypixelMessage else thePlayerNeverJoinedHypixelMessage) =<< hypixelSafeRequestStats uuid
    dailyHypixelBowStats <- getHypixelBowTimeStatsByUUID DailyStats uuid
    weeklyHypixelBowStats <- getHypixelBowTimeStatsByUUID WeeklyStats uuid
    monthlyHypixelBowStats <- getHypixelBowTimeStatsByUUID MonthlyStats uuid
    return FullHypixelBowTimeStats {..}
  , shouldAddAccount = (>= 50) . bowWins . currentHypixelBowStats
  , updateStats = \uuid stats -> do
      void $ setHypixelBowLeaderboardEntryByUUID uuid (hypixelBowStatsToLeaderboards (currentHypixelBowStats stats))
      applyRolesDivisionTitleByUUID uuid
      announceMilestones
  , showStats = showFullHypixelBowTimeStats
  }