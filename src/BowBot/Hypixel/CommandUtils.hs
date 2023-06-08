module BowBot.Hypixel.CommandUtils where

import BowBot.Command
import BowBot.Hypixel.Stats
import BowBot.Utils
import BowBot.Hypixel.Basic (HypixelApi(..))
import BowBot.Counter.Basic
import BowBot.Hypixel.Leaderboard
import BowBot.Minecraft.Basic
import Control.Monad.Except

youNeverJoinedHypixelMessage :: Text
youNeverJoinedHypixelMessage = "*You have never joined Hypixel!*"

thePlayerNeverJoinedHypixelMessage :: Text
thePlayerNeverJoinedHypixelMessage = "*The player has never joined Hypixel!*"

hypixelSafeRequestStats :: UUID -> ExceptT Text CommandHandler (Maybe HypixelBowStats)
hypixelSafeRequestStats uuid = do
  cv <- tryIncreaseCounter HypixelApi 1
  case cv of
    Nothing -> do
      oldstats <- getHypixelBowLeaderboardEntryByUUID uuid
      fmap (`completeHypixelBowStats` oldstats) <$> requestHypixelBowStats uuid
    Just sec -> throwError $ "*Too many requests! Wait another " <> showt sec <> " seconds!*"
