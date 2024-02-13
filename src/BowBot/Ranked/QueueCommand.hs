{-# LANGUAGE QuasiQuotes #-}

module BowBot.Ranked.QueueCommand where

import BowBot.Command
import BowBot.Discord.Utils
import BowBot.Account.Basic
import BowBot.Command.Utils
import BowBot.Ranked.Queue
import BowBot.Ranked.Stats
import Control.Monad.Except (throwError)
import BowBot.Ranked.Game
import BowBot.DB.Basic
import BowBot.Minecraft.Basic
import BowBot.Minecraft.Account
import qualified Data.Text as T

queueCommand :: Command
queueCommand = Command CommandInfo
  { commandName = "q"
  , commandHelpEntries = [HelpEntry { helpUsage = "q", helpDescription = "enter the Ranked Bow queue", helpGroup = "ranked" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ noArguments $ do
    did <- userId <$> envs envSender
    bid <- liftMaybe youArentRegisteredMessage =<< getBowBotIdByDiscord did
    stats <- getRankedBowStatsByBowBot bid
    when (isNothing stats) $ do
      uuid <- liftMaybe youArentRegisteredMessage =<< getSelectedMinecraftUUIDByBowBotId bid
      r <- addRankedPlayer bid uuid
      unless r $ throwError somethingWentWrongMessage
    r <- liftMaybe somethingWentWrongMessage =<< addToQueue 2 bid
    case r of
      AlreadyInQueue -> respond "*You are already in this queue!*"
      AddedToQueue n -> do
        MinecraftAccount {..} <- liftMaybe somethingWentWrongMessage =<< queryOnlyLog [mysql|SELECT MinecraftAccount FROM `minecraft` JOIN `ranked_bow_stats` ON `ranked_uuid` = `minecraft`.`uuid` WHERE `account_id` = bid|]
        respond $ "**" <> discordEscape (head mcNames) <> " joined the queue!**\nCurrently **" <> showt n <> "/2** people in this queue."
      CurrentlyInGame n -> respond $ "*You are currently in game #" <> showt n <> "!*"
      QueueFilled [p1, p2] -> do
        gameId <- liftMaybe somethingWentWrongMessage =<< createRankedGame (p1, p2)
        mcAccounts <- queryLog [mysql|SELECT `account_id`, MinecraftAccount FROM `ranked_bow_stats` JOIN `minecraft` ON `uuid` = `ranked_bow_stats`.`ranked_uuid` WHERE `ranked_bow_stats`.`current_game` = gameId|]
        discords <- queryLog [mysql|SELECT `account_discord`.`account_id`, `discord_id` FROM `account_discord` JOIN `ranked_bow_stats` ON `account_id` = `account_discord`.`account_id` WHERE `ranked_bow_stats`.`current_game` = gameId|]
        elos <- queryLog [mysql|SELECT `account_id`, `elo` FROM `ranked_bow_stats` WHERE `ranked_bow_stats`.`current_game` = gameId|]
        let formatPlayer p = "**" <> discordEscape (head (maybe [] mcNames $ lookup p mcAccounts)) <> "** (" <> maybe "" showt (lookup p elos) <> ") " <> T.unwords (map (\(_, i) -> "<@" <> showt i <> ">") $ filter ((==p) . fst) discords)
        respond $ "**Ranked Bow Duels Game #" <> showt gameId <> " created!**\n" <> formatPlayer p1 <> " vs. " <> formatPlayer p2
      QueueFilled _ -> respond somethingWentWrongMessage

leaveCommand :: Command
leaveCommand = Command CommandInfo
  { commandName = "l"
  , commandHelpEntries = [HelpEntry { helpUsage = "l", helpDescription = "leave the Ranked Bow queue", helpGroup = "ranked" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ noArguments $ do
    did <- userId <$> envs envSender
    bid <- liftMaybe youArentRegisteredMessage =<< getBowBotIdByDiscord did
    r <- removeFromQueue bid
    MinecraftAccount {..} <- liftMaybe somethingWentWrongMessage =<< queryOnlyLog [mysql|SELECT MinecraftAccount FROM `minecraft` JOIN `ranked_bow_stats` ON `ranked_uuid` = `minecraft`.`uuid` WHERE `account_id` = bid|]
    respond $ case r of
      Just n -> "**" <> discordEscape (head mcNames) <> " left the queue!**\nCurrently **" <> showt n <> "/2** people in this queue."
      Nothing -> "*You are not in this queue!*"

listQueueCommand :: Command
listQueueCommand = Command CommandInfo
  { commandName = "qlist"
  , commandHelpEntries = [HelpEntry { helpUsage = "qlist", helpDescription = "list the Ranked Bow queue", helpGroup = "ranked" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ noArguments $ do
    q <- queryLog [mysql|SELECT MinecraftAccount FROM `ranked_bow_stats` JOIN `minecraft` ON `uuid` = `ranked_uuid` WHERE `queue`|]
    respond $ case q of
      [] -> "*This queue is currently empty!*"
      _ -> "**The Ranked Bow Duels queue:**\n```" <> T.unwords (map (head . mcNames) q) <> "```"