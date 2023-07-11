module BowBot.Hypixel.Leaderboard.Command(
  module BowBot.Hypixel.Leaderboard.Command, leaderboardCommand
) where

import BowBot.Hypixel.Leaderboard
import Control.Monad.Except
import BowBot.Discord.Utils
import BowBot.Leaderboard.Command
import BowBot.Command
import BowBot.Minecraft.Basic
import BowBot.Hypixel.Guild

winsLeaderboardType :: LeaderboardType HypixelBowLeaderboardEntry Integer
winsLeaderboardType = LeaderboardType "Hypixel Bow Duels Wins" (pure (const True)) "Wins" showt getHypixelBowLeaderboards (filterMaybe (>= 500) . bowLbWins)

lossesLeaderboardType :: LeaderboardType HypixelBowLeaderboardEntry Integer
lossesLeaderboardType = LeaderboardType "Hypixel Bow Duels Losses" (pure (const True)) "Losses" showt getHypixelBowLeaderboards (fmap bowLbLosses . filterMaybe ((>= 500) . bowLbWins))

winstreakLeaderboardType :: LeaderboardType HypixelBowLeaderboardEntry Integer
winstreakLeaderboardType = LeaderboardType "Hypixel Bow Duels Winstreak" (pure (const True)) "Winstreak" showt getHypixelBowLeaderboards (filterMaybe (>= 50) <=< bowLbWinstreak)

wlrLeaderboardType :: LeaderboardType HypixelBowLeaderboardEntry (WLR Integer)
wlrLeaderboardType = LeaderboardType "Hypixel Bow Duels WLR" (pure (const True)) "WLR" showWLR getHypixelBowLeaderboards (fmap bowLbWLR . filterMaybe requirements)
  where
    requirements HypixelBowLeaderboardEntry {..} = bowLbWins >= bowLbLosses && bowLbWins >= 150

leaderboardGuildFilter :: CommandHandler (UUID -> Bool)
leaderboardGuildFilter = do
  mems <- getHypixelGuildMembers
  return (`elem` mems)

winsLeaderboardTypeGuild :: LeaderboardType HypixelBowLeaderboardEntry Integer
winsLeaderboardTypeGuild = LeaderboardType "Hypixel Bow Duels Guild Wins" leaderboardGuildFilter "Wins" showt getHypixelBowLeaderboards (Just . bowLbWins)

lossesLeaderboardTypeGuild :: LeaderboardType HypixelBowLeaderboardEntry Integer
lossesLeaderboardTypeGuild = LeaderboardType "Hypixel Bow Duels Guild Losses" leaderboardGuildFilter "Losses" showt getHypixelBowLeaderboards (Just . bowLbLosses)

winstreakLeaderboardTypeGuild :: LeaderboardType HypixelBowLeaderboardEntry Integer
winstreakLeaderboardTypeGuild = LeaderboardType "Hypixel Bow Duels Guild Winstreak" leaderboardGuildFilter "Winstreak" showt getHypixelBowLeaderboards bowLbWinstreak

wlrLeaderboardTypeGuild :: LeaderboardType HypixelBowLeaderboardEntry (WLR Integer)
wlrLeaderboardTypeGuild = LeaderboardType "Hypixel Bow Duels Guild WLR" leaderboardGuildFilter "WLR" showWLR getHypixelBowLeaderboards (Just . bowLbWLR)
