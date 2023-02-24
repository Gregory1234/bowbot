module BowBot.Hypixel.StatsCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Minecraft.Arg
import BowBot.Hypixel.Stats
import BowBot.Settings.Basic
import BowBot.Utils
import BowBot.Hypixel.Basic (HypixelApi(..))
import BowBot.Counter.Basic
import Control.Monad.Error.Class (throwError)
import Discord.Types
import BowBot.Hypixel.Leaderboard
import BowBot.Hypixel.Announce
import BowBot.BotData.Cached (storeInCacheIndexed, storeInCache, getFromCache)
import BowBot.BotData.Info
import BowBot.Discord.Roles
import BowBot.Account.Basic
import BowBot.Discord.Utils (discordGuildMembers)
import BowBot.Command.Tips
import BowBot.Hypixel.LeaderboardStatus

hypixelStatsCommand :: SettingsSource -> Text -> Text -> Command
hypixelStatsCommand src name desc = Command CommandInfo
  { commandName = name
  , commandHelpEntries = [HelpEntry { helpUsage = name <> " [name]", helpDescription = desc, helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument (minecraftArgFullConstraintWithSkipTip helper) $ \(MinecraftResponse {mcResponseAccount = mcResponseAccount@MinecraftAccount {..}, ..}, stats) -> do
    let (didYouMean, renderedName) = (if mcResponseAutocorrect == ResponseAutocorrect then "*Did you mean* " else "", showMinecraftAccountDiscord mcResponseTime mcResponseAccount)
    user <- envs envSender
    settings <- getSettingsFromSource src (userId user)
    let addAccount = bowWins stats >= 50 && mcResponseAutocorrect == ResponseNew
    when addAccount $ minecraftNewAccountTip mcResponseAccount
    respond $ didYouMean <> renderedName <> ":\n" <> showHypixelBowStats settings stats
    when addAccount $ do
      a <- storeInCache [mcResponseAccount]
      when a $ void $ setHypixelBowLeaderboardEntryByUUID mcUUID (hypixelBowStatsToLeaderboards stats)
    when (mcResponseAutocorrect /= ResponseNew) $ do
      isBanned <- getHypixelIsBannedByUUID mcUUID
      when (isBanned == NotBanned) $ do
        void $ setHypixelBowLeaderboardEntryByUUID mcUUID (hypixelBowStatsToLeaderboards stats)
        applyRolesDivisionTitleByUUID mcUUID
        announceMilestones
  where
    helper MinecraftAccount {..} = do
      cv <- tryIncreaseCounter HypixelApi 1
      case cv of
        Nothing -> do
          stats <- liftMaybe "*The player has never joined Hypixel!*" =<< requestHypixelBowStats mcUUID
          oldstats <- getHypixelBowLeaderboardEntryByUUID mcUUID
          return (if bowWins stats + bowLosses stats /= 0 then ResponseGood else ResponseFindBetter, completeHypixelBowStats stats oldstats)
        Just sec -> throwError $ "*Too many requests! Wait another " <> showt sec <> " seconds!*"