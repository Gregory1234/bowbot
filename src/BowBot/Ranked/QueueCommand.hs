module BowBot.Ranked.QueueCommand where

import BowBot.Command
import BowBot.Discord.Utils
import BowBot.Account.Basic
import BowBot.Command.Utils
import BowBot.Ranked.Queue
import BowBot.Ranked.Stats
import Control.Monad.Except (throwError)

queueCommand :: Command
queueCommand = Command CommandInfo
  { commandName = "rq"
  , commandHelpEntries = [HelpEntry { helpUsage = "rq", helpDescription = "enter the Ranked Bow queue", helpGroup = "ranked" }]
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
      AlreadyInQueue -> respond "Already in queue!"
      AddedToQueue -> respond "Added to queue!"
      CurrentlyInGame -> respond "Already in game!"
      QueueFilled players -> respond $ "Created game with players: " <> pack (show players)

leaveCommand :: Command
leaveCommand = Command CommandInfo
  { commandName = "rl"
  , commandHelpEntries = [HelpEntry { helpUsage = "rl", helpDescription = "leave the Ranked Bow queue", helpGroup = "ranked" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ noArguments $ do
    did <- userId <$> envs envSender
    bid <- liftMaybe youArentRegisteredMessage =<< getBowBotIdByDiscord did
    r <- removeFromQueue bid
    respond $ if r then "Left queue!" else "You are not in queue!"