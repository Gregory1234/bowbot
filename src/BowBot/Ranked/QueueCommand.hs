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
import BowBot.Account.Utils
import BowBot.BotData.Info

queueCommand :: Command
queueCommand = Command CommandInfo
  { commandName = "q"
  , commandHelpEntries = [HelpEntry { helpUsage = "q [queue]", helpDescription = "enter a Ranked Bow queue", helpGroup = "ranked" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument $ \case
    (Just qName) -> do
      queue <- liftMaybe "*Bad queue name!*" =<< getQueueByName qName
      did <- userId <$> envs envSender
      bid <- liftMaybe youArentRegisteredMessage =<< getBowBotIdByDiscord did
      acc <- getOrMakeRankedAccount bid
      stats <- getRankedBowStatsByBowBot queue bid
      when (isNothing stats) $ do
        r <- addRankedPlayerQueue queue bid
        unless r $ throwError somethingWentWrongMessage
      r <- liftMaybe somethingWentWrongMessage =<< addToQueue 2 queue bid
      case r of
        AlreadyInQueue -> respond "*You are already in this queue!*"
        AddedToQueue n -> do
          respond $ "**" <> discordEscape (head (mcNames acc)) <> " joined the queue!**\nCurrently **" <> showt n <> "/2** people in this queue."
        CurrentlyInGame n -> respond $ "*You are currently in game #" <> showt n <> "!*"
        QueueFilled [p1, p2] -> createGame queue p1 p2
        QueueFilled _ -> respond somethingWentWrongMessage
    Nothing -> do
      did <- userId <$> envs envSender
      bid <- liftMaybe youArentRegisteredMessage =<< getBowBotIdByDiscord did
      acc <- getOrMakeRankedAccount bid
      allQueues <- askInfo rankedBowQueuesInfo
      currentQueues <- getCurrentQueuesByBowBotId bid
      let queuesToJoin = allQueues \\ currentQueues
      when (null queuesToJoin) $ throwError "*You are already in all queues!*"
      for_ queuesToJoin $ \queue -> do
        stats <- getRankedBowStatsByBowBot queue bid
        when (isNothing stats) $ do
          r <- addRankedPlayerQueue queue bid
          unless r $ throwError somethingWentWrongMessage
      r <- addToQueueMany (map (, 2) queuesToJoin) bid
      case r of
        AddedToQueueMany vs -> do
          respond $ "**" <> discordEscape (head (mcNames acc)) <> " joined the queues: " <> T.intercalate ", " (map queueName queuesToJoin) 
            <> "!**\nCurrent queue status:\n" <> T.unlines ["**" <> showt n <> "/2** people in " <> queueName q <> " queue." | (q, n) <- vs]
        CurrentlyInGameSome n -> respond $ "*You are currently in game #" <> showt n <> "!*"
        QueueFilledSome queue [p1, p2] -> createGame queue p1 p2
        QueueFilledSome _ _ -> respond somethingWentWrongMessage
  where
    getOrMakeRankedAccount bid = do
      a <- getRankedMinecraftAccountByBowBotId bid
      case a of
        Nothing -> do
          mc <- liftMaybe youArentRegisteredMessage =<< getSelectedMinecraftByBowBotId bid
          c <- addRankedPlayer bid (mcUUID mc)
          unless c $ throwError somethingWentWrongMessage
          return mc
        Just mc -> return mc
    createGame queue p1 p2 = do
      gameId <- liftMaybe somethingWentWrongMessage =<< createRankedGame queue (p1, p2)
      mcAccounts <- queryLog [mysql|SELECT `account_id`, MinecraftAccount FROM `ranked_bow` JOIN `minecraft` ON `uuid` = `ranked_bow`.`ranked_uuid` WHERE `ranked_bow`.`current_game` = gameId|]
      discords <- queryLog [mysql|SELECT `account_discord`.`account_id`, `discord_id` FROM `account_discord` JOIN `ranked_bow` ON `account_id` = `account_discord`.`account_id` WHERE `ranked_bow`.`current_game` = gameId|]
      elos <- queryLog [mysql|SELECT `ranked_bow_stats`.`account_id`, `elo` FROM `ranked_bow_stats` JOIN `ranked_bow` ON `account_id` = `ranked_bow_stats`.`account_id` WHERE `ranked_bow`.`current_game` = gameId AND `queue` = queue|]
      let formatPlayer p = "**" <> discordEscape (head (maybe [] mcNames $ lookup p mcAccounts)) <> "** (" <> maybe "" showt (lookup p elos) <> ") " <> T.unwords (map (\(_, i) -> "<@" <> showt i <> ">") $ filter ((==p) . fst) discords)
      respond $ "**Ranked Bow Duels Game #" <> showt gameId <> " in queue " <> queueName queue <> " created!**\n" <> formatPlayer p1 <> " vs. " <> formatPlayer p2

leaveCommand :: Command
leaveCommand = Command CommandInfo
  { commandName = "l"
  , commandHelpEntries = [HelpEntry { helpUsage = "l", helpDescription = "leave all Ranked Bow queues", helpGroup = "ranked" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ noArguments $ do
    did <- userId <$> envs envSender
    bid <- liftMaybe youArentRegisteredMessage =<< getBowBotIdByDiscord did
    r <- removeFromAllQueues bid
    MinecraftAccount {..} <- liftMaybe somethingWentWrongMessage =<< getRankedMinecraftAccountByBowBotId bid
    respond $ case r of
      Just vs -> "**" <> discordEscape (head mcNames) <> " left all queues!**\nCurrent queue status:\n" <> (if null vs then "All queues are empty!" else T.unlines ["**" <> showt n <> "/2** people in " <> queueName q <> " queue." | (q, n) <- vs])
      Nothing -> "*You are not in this queue!*"

listQueueCommand :: Command
listQueueCommand = Command CommandInfo
  { commandName = "qlist"
  , commandHelpEntries = [HelpEntry { helpUsage = "qlist [queue]", helpDescription = "list the Ranked Bow queue", helpGroup = "ranked" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneArgument $ \qName -> do
    queue <- liftMaybe "*Bad queue name!*" =<< getQueueByName qName
    q <- queryLog [mysql|SELECT MinecraftAccount FROM `ranked_bow` JOIN `minecraft` ON `uuid` = `ranked_uuid` JOIN `ranked_bow_stats` ON `account_id` = `ranked_bow`.`account_id` WHERE `queue` = queue AND `in_queue`|]
    respond $ case q of
      [] -> "*This queue is currently empty!*"
      _ -> "**The Ranked Bow Duels queue:**\n```" <> T.unwords (map (head . mcNames) q) <> "```"