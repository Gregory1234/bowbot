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

youNeverPlayerRankedBowDuels :: Text
youNeverPlayerRankedBowDuels = "*You have never played Ranked Bow Duels!*"

thePlayerHasNeverPlayerRankedBowDuels :: Text
thePlayerHasNeverPlayerRankedBowDuels = "*The player has never played Ranked Bow Duels!*"

rankedBowStatsCommand :: SettingsSource -> Text -> Text -> Command
rankedBowStatsCommand src name desc = Command CommandInfo
  { commandName = name
  , commandHelpEntries = [HelpEntry { helpUsage = name <> " [name]", helpDescription = desc, helpGroup = "ranked" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument $ \case
    Nothing -> do
      did <- userId <$> envs envSender
      (uuid, stats) <- liftMaybe youNeverPlayerRankedBowDuels =<< getStatsByDiscord did
      acc <- liftMaybe thePlayerDoesNotExistMessage =<< getMinecraftAccountByUUID uuid
      displayStats (minecraftAccountToHeader acc Nothing) stats
    Just (uuidFromString -> Just uuid) -> do
      stats <- liftMaybe thePlayerHasNeverPlayerRankedBowDuels =<< getStatsByUUID uuid
      acc <- liftMaybe thePlayerDoesNotExistMessage =<< getMinecraftAccountByUUID uuid
      displayStats (minecraftAccountToHeader acc Nothing) stats
    Just (discordIdFromString -> Just did) -> do
      (uuid, stats) <- liftMaybe thePlayerHasNeverPlayerRankedBowDuels =<< getStatsByDiscord did
      acc <- liftMaybe thePlayerDoesNotExistMessage =<< getMinecraftAccountByUUID uuid
      displayStats (minecraftAccountToHeader acc Nothing) stats
    Just n -> do
      people <- queryLog [mysql|SELECT MinecraftAccount, RankedBowStats FROM `ranked_bow_stats` JOIN `minecraft` ON `uuid` = `ranked_uuid`|]
      ac <- liftMaybe thePlayerDoesNotExistMessage $ minecraftAutocorrectGeneral (map fst people) n
      stats <- liftMaybe somethingWentWrongMessage $ lookup (autocorrectAccount ac) people
      displayStats (minecraftAutocorrectToHeader ac) stats
  where
    getStatsByDiscord did = queryOnlyLog [mysql|SELECT `ranked_uuid`, RankedBowStats FROM `ranked_bow_stats` JOIN `account_discord` ON `account_id` = `ranked_bow_stats`.`account_id` WHERE `account_discord`.`discord_id` = did|]
    getStatsByUUID uuid = queryOnlyLog [mysql|SELECT RankedBowStats FROM `ranked_bow_stats` WHERE `ranked_uuid` = uuid|]
    displayStats header stats = do
      user <- envs envSender
      settings <- getSettingsFromSource src (userId user)
      respond $ header <> showRankedBowStats settings stats