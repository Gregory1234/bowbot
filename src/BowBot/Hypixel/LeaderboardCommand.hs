module BowBot.Hypixel.LeaderboardCommand where

import BowBot.Command
import BowBot.Hypixel.Leaderboard
import BowBot.Minecraft.Account
import BowBot.Account.Basic
import Control.Monad.Except
import BowBot.Minecraft.Basic
import Discord.Types
import BowBot.BotData.Cached
import BowBot.Discord.Utils
import qualified Data.HashMap.Strict as HM
import Data.Either (isRight)
import BowBot.Discord.Account
import BowBot.Hypixel.Guild
import qualified Data.Text as T

data LeaderboardType = LeaderboardType { leaderboardName :: !Text, leaderboardStatName :: !Text, leaderboardParser :: HypixelBowLeaderboardEntry -> Maybe (Integer, Text) }

thePlayerIsntOnThisLeaderboardMessage :: Text
thePlayerIsntOnThisLeaderboardMessage = "*The player isn't on this leaderboard!*"

leaderboardCommand :: LeaderboardType -> Text -> Command
leaderboardCommand lbt@LeaderboardType {..} name = Command CommandInfo
  { commandName = name
  , commandHelpEntries =
    [ HelpEntry { helpUsage = name <> " [name|page|\"all\"]", helpDescription = "show Bow Duels " <> leaderboardName <> " leaderboard", helpGroup = "normal" } ]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument $ do
  undefined

-- TODO: refactor all of this!!!

leaderboardGuildCommand :: LeaderboardType -> Text -> Command
leaderboardGuildCommand lbt@LeaderboardType {..} name = Command CommandInfo
  { commandName = name
  , commandHelpEntries =
    [ HelpEntry { helpUsage = name <> " [name|page|\"all\"]", helpDescription = "show Bow Duels " <> leaderboardName <> " guild leaderboard", helpGroup = "normal" } ]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument $ do
    undefined

winsLeaderboardType :: LeaderboardType
winsLeaderboardType = LeaderboardType "Hypixel Bow Duels Wins" "Wins" $ \case
  HypixelBowLeaderboardEntry {..} | bowLbWins >= 500 -> Just (bowLbWins, showt bowLbWins)
  _ -> Nothing

lossesLeaderboardType :: LeaderboardType
lossesLeaderboardType = LeaderboardType "Hypixel Bow Duels Losses" "Losses" $ \case
  HypixelBowLeaderboardEntry {..} | bowLbWins >= 500 -> Just (bowLbLosses, showt bowLbLosses)
  _ -> Nothing

winstreakLeaderboardType :: LeaderboardType
winstreakLeaderboardType = LeaderboardType "Hypixel Bow Duels Winstreak" "Winstreak" $ \case
  HypixelBowLeaderboardEntry { bowLbWinstreak = (Just ws) } | ws >= 50 -> Just (ws, showt ws)
  _ -> Nothing

wlrLeaderboardType :: LeaderboardType
wlrLeaderboardType = LeaderboardType "Hypixel Bow Duels WLR" "WLR" $ \case
  HypixelBowLeaderboardEntry {..} | bowLbWins >= bowLbLosses, bowLbWins >= 150 -> Just (if bowLbLosses == 0 then bowLbWins*100000000 else (bowLbWins*10000) `div` bowLbLosses, showWLR bowLbWins bowLbLosses)
  _ -> Nothing

winsLeaderboardTypeUnrestricted :: LeaderboardType
winsLeaderboardTypeUnrestricted = LeaderboardType "Hypixel Bow Duels Wins" "Wins" $ \HypixelBowLeaderboardEntry {..} -> Just (bowLbWins, showt bowLbWins)

lossesLeaderboardTypeUnrestricted :: LeaderboardType
lossesLeaderboardTypeUnrestricted = LeaderboardType "Hypixel Bow Duels Losses" "Losses" $ \HypixelBowLeaderboardEntry {..} -> Just (bowLbLosses, showt bowLbLosses)

winstreakLeaderboardTypeUnrestricted :: LeaderboardType
winstreakLeaderboardTypeUnrestricted = LeaderboardType "Hypixel Bow Duels Winstreak" "Winstreak" $ \HypixelBowLeaderboardEntry {..} -> fmap (\ws -> (ws, showt ws)) bowLbWinstreak

wlrLeaderboardTypeUnrestricted :: LeaderboardType
wlrLeaderboardTypeUnrestricted = LeaderboardType "Hypixel Bow Duels WLR" "WLR" $ \HypixelBowLeaderboardEntry {..} -> Just (if bowLbLosses == 0 then bowLbWins*100000000 else (bowLbWins*10000) `div` bowLbLosses, showWLR bowLbWins bowLbLosses)