{-# LANGUAGE QuasiQuotes #-}

module BowBot.Ranked.StatsCommand where

import BowBot.Command
import BowBot.Settings.Basic
import BowBot.Discord.Utils
import BowBot.Command.Utils
import BowBot.Minecraft.Account
import BowBot.DB.Basic
import BowBot.Ranked.Stats
import BowBot.Minecraft.Basic
import BowBot.Account.Basic
import BowBot.Ranked.Queue
import qualified Data.Text as T

youNeverPlayerRankedBowDuels :: Text
youNeverPlayerRankedBowDuels = "*You have never played Ranked Bow Duels!*"

thePlayerHasNeverPlayerRankedBowDuels :: Text
thePlayerHasNeverPlayerRankedBowDuels = "*The player has never played Ranked Bow Duels!*"

rankedBowStatsCommand :: SettingsSource -> Text -> Text -> Command
rankedBowStatsCommand src name desc = Command CommandInfo
  { commandName = name
  , commandHelpEntries =
    [ HelpEntry { helpUsage = name <> " [name]", helpDescription = desc, helpGroup = "ranked" }
    , HelpEntry { helpUsage = name <> " [queue] [name]", helpDescription = desc, helpGroup = "ranked" } ]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ twoOptionalArguments $ \case
    Nothing -> do
      did <- userId <$> envs envSender
      bid <- liftMaybe youArentRegisteredMessage =<< getBowBotIdByDiscord did
      stats <- getStatsByDiscord did
      acc <- liftMaybe youNeverPlayerRankedBowDuels =<< getRankedMinecraftAccountByBowBotId bid
      displayStats (minecraftAccountToHeader acc Nothing) stats
    Just (uuidFromString -> Just uuid, Nothing) -> do
      stats <- getStatsByUUID uuid
      acc <- liftMaybe thePlayerDoesNotExistMessage =<< getMinecraftAccountByUUID uuid
      displayStats (minecraftAccountToHeader acc Nothing) stats
    Just (discordIdFromString -> Just did, Nothing) -> do
      stats <- getStatsByDiscord did
      bid <- liftMaybe youArentRegisteredMessage =<< getBowBotIdByDiscord did
      acc <- liftMaybe youNeverPlayerRankedBowDuels =<< getRankedMinecraftAccountByBowBotId bid
      displayStats (minecraftAccountToHeader acc Nothing) stats
    Just (n, Nothing) -> do
      maybeQueue <- getQueueByName n
      case maybeQueue of
        Just queue -> do
          did <- userId <$> envs envSender
          bid <- liftMaybe youArentRegisteredMessage =<< getBowBotIdByDiscord did
          stats <- getQueueStatsByDiscord queue did
          acc <- liftMaybe youNeverPlayerRankedBowDuels =<< getRankedMinecraftAccountByBowBotId bid
          displayStatsQueue (minecraftAccountToHeader acc Nothing) stats
        Nothing -> do
          people <- queryLog [mysql|SELECT MinecraftAccount FROM `ranked_bow` JOIN `minecraft` ON `uuid` = `ranked_uuid`|]
          ac <- liftMaybe thePlayerDoesNotExistMessage $ minecraftAutocorrectGeneral people n
          stats <- getStatsByUUID (mcUUID $ autocorrectAccount ac)
          displayStats (minecraftAutocorrectToHeader ac) stats
    Just (qName, Just (uuidFromString -> Just uuid)) -> do
      queue <- liftMaybe "*Bad queue name!*" =<< getQueueByName qName
      stats <- getQueueStatsByUUID queue uuid
      acc <- liftMaybe thePlayerDoesNotExistMessage =<< getMinecraftAccountByUUID uuid
      displayStatsQueue (minecraftAccountToHeader acc Nothing) stats
    Just (qName, Just (discordIdFromString -> Just did)) -> do
      queue <- liftMaybe "*Bad queue name!*" =<< getQueueByName qName
      stats <- getQueueStatsByDiscord queue did
      bid <- liftMaybe youArentRegisteredMessage =<< getBowBotIdByDiscord did
      acc <- liftMaybe youNeverPlayerRankedBowDuels =<< getRankedMinecraftAccountByBowBotId bid
      displayStatsQueue (minecraftAccountToHeader acc Nothing) stats
    Just (qName, Just n) -> do
      queue <- liftMaybe "*Bad queue name!*" =<< getQueueByName qName
      people <- queryLog [mysql|SELECT MinecraftAccount FROM `ranked_bow` JOIN `minecraft` ON `uuid` = `ranked_uuid`|]
      ac <- liftMaybe thePlayerDoesNotExistMessage $ minecraftAutocorrectGeneral people n
      stats <- getQueueStatsByUUID queue (mcUUID $ autocorrectAccount ac)
      displayStatsQueue (minecraftAutocorrectToHeader ac) stats
  where
    getStatsByDiscord did = queryLog [mysql|SELECT RankedBowStats FROM `ranked_bow_stats` JOIN `account_discord` ON `account_id` = `ranked_bow_stats`.`account_id` WHERE `account_discord`.`discord_id` = did|]
    getStatsByUUID uuid = queryLog [mysql|SELECT RankedBowStats FROM `ranked_bow_stats` JOIN `ranked_bow` ON `account_id` = `ranked_bow_stats`.`account_id` WHERE `ranked_uuid` = uuid|]
    getQueueStatsByDiscord queue did = queryOnlyLog [mysql|SELECT RankedBowStats FROM `ranked_bow_stats` JOIN `account_discord` ON `account_id` = `ranked_bow_stats`.`account_id` WHERE `account_discord`.`discord_id` = did AND `queue` = queue|]
    getQueueStatsByUUID queue uuid = queryOnlyLog [mysql|SELECT RankedBowStats FROM `ranked_bow_stats` JOIN `ranked_bow` ON `account_id` = `ranked_bow_stats`.`account_id` WHERE `ranked_uuid` = uuid AND `queue` = queue|]
    displayStats header stats = do
      user <- envs envSender
      settings <- getSettingsFromSource src (userId user)
      respond $ header <> T.unlines (showRankedBowStats settings <$> stats)
    displayStatsQueue header stats = do
      user <- envs envSender
      settings <- getSettingsFromSource src (userId user)
      respond $ header <> maybe "*The player never played in this queue!*" (showRankedBowStats settings) stats