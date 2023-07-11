module BowBot.Hypixel.LeaderboardCommand where

import BowBot.Command
import BowBot.Hypixel.Leaderboard
import BowBot.Minecraft.Account
import BowBot.Account.Basic
import Control.Monad.Except
import BowBot.Minecraft.Basic
import Discord.Types
import BowBot.Discord.Utils
import qualified Data.HashMap.Strict as HM
import BowBot.Hypixel.Guild
import qualified Data.Text as T
import Data.Bifunctor (second)
import BowBot.Command.Utils
import Data.Ord
import BowBot.DB.Typed

data LeaderboardType s a = LeaderboardType
  { leaderboardName :: !Text
  , leaderboardIsGuild :: !Bool
  , leaderboardStatName :: !Text
  , leaderboardShowValue :: a -> Text
  , leaderboardGetStats :: CommandHandler (HM.HashMap UUID s)
  , leaderboardParser :: s -> Maybe a
  }

leaderboardDescription :: LeaderboardType s a -> Text
leaderboardDescription LeaderboardType {..} = "show Bow Duels " <> leaderboardName <> (if leaderboardIsGuild then " guild" else "") <> " leaderboard"

thePlayerIsntOnThisLeaderboardMessage :: Text
thePlayerIsntOnThisLeaderboardMessage = "*The player isn't on this leaderboard!*"

data LeaderboardRow a = LeaderboardRow
  { lbRowAccount :: MinecraftAccount
  , lbRowIndex :: Int
  , lbRowValue :: a
  , lbRowSelected :: Bool
  }

showLeaderboardRow :: LeaderboardType s a -> LeaderboardRow a -> Text
showLeaderboardRow LeaderboardType {..} LeaderboardRow {..} =
     pad 5 (showt lbRowIndex <> ".")
  <> (if lbRowSelected then "*" else " ")
  <> pad 20 (head (mcNames lbRowAccount))
  <> " ( " <> leaderboardShowValue lbRowValue <> " " <> leaderboardStatName <> " )"

leaderboardCommand :: forall s a. Ord a => LeaderboardType s a -> Text -> Command
leaderboardCommand lbt@LeaderboardType {..} name = Command CommandInfo
  { commandName = name
  , commandHelpEntries =
    [ HelpEntry { helpUsage = name <> " [name|page|\"all\"]", helpDescription = leaderboardDescription lbt, helpGroup = "normal" } ]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument $ \case
    Just "all" -> do
      lb <- generateLeaderboardDiscordSelf
      respondFile (name <> ".txt") $ T.unlines (map (showLeaderboardRow lbt) lb)
    Just (readMaybe @Int . unpack -> Just page) | page < 1000 -> do
      lb <- generateLeaderboardDiscordSelf
      displayLeaderboard (Just (page - 1)) "" lb
    Nothing -> do
      lb <- generateLeaderboardDiscordSelf
      displayLeaderboard Nothing "" lb
    Just (uuidFromString -> Just uuid) -> do
      acc <- liftMaybe thePlayerDoesNotExistMessage =<< getMinecraftAccountByUUID uuid
      handlerMinecraft (autocorrectFromAccountDirect acc)
    Just (discordIdFromString -> Just did) -> do
      mcs <- getMinecraftUUIDsByDiscord did
      lb <- generateLeaderboard mcs
      displayLeaderboard Nothing "" lb
    Just n -> do
      ac <- liftMaybe thePlayerIsntOnThisLeaderboardMessage =<< minecraftAutocorrect n
      showSelfSkipTip (autocorrectAccount ac)
      handlerMinecraft ac
  where
    generateLeaderboardDiscordSelf :: ExceptT Text CommandHandler [LeaderboardRow a]
    generateLeaderboardDiscordSelf = do
      did <- userId <$> envs envSender
      mcs <- getMinecraftUUIDsByDiscord did
      generateLeaderboard mcs
    generateLeaderboard :: [UUID] -> ExceptT Text CommandHandler [LeaderboardRow a]
    generateLeaderboard selected = do
      lbFull <- HM.toList <$> lift leaderboardGetStats
      lb <- if leaderboardIsGuild then do
                gmems <- getHypixelGuildMembers
                return $ filter ((`elem` gmems) . fst) lbFull
              else return lbFull
      accs <- HM.fromList . map (\x -> (mcUUID x, x)) <$> queryLogT_ selectAllQuery'
      let lbParsed = mapMaybe (sequence . second leaderboardParser) lb
      let lbSorted = sortOn (Down . snd) lbParsed
      return $ zipWith (\lbRowIndex (uuid, lbRowValue) -> LeaderboardRow {lbRowAccount = accs HM.! uuid, lbRowSelected = uuid `elem` selected, ..}) [1..] lbSorted
    handlerMinecraft :: MinecraftAutocorrect -> ExceptT Text CommandHandler ()
    handlerMinecraft ac@MinecraftAutocorrect {..} = do
      lb <- generateLeaderboard [mcUUID autocorrectAccount]
      unless (any lbRowSelected lb) $ throwError thePlayerIsntOnThisLeaderboardMessage
      displayLeaderboard Nothing (if autocorrectIsDirect then "" else minecraftAutocorrectToHeader ac) lb
    displayLeaderboard :: Maybe Int -> Text -> [LeaderboardRow a] -> ExceptT Text CommandHandler ()
    displayLeaderboard page headerExtra lb = do
      let pages = chunksOf 20 lb
      let pagenum = fromMaybe 0 $ page <|> findIndex (any lbRowSelected) pages
      when (pagenum < 0 || pagenum >= length pages) $ throwError $ "*Wrong page number, it has to be between **1** and **" <> showt (length pages) <> "**.*"
      respond $ headerExtra
             <> leaderboardName <> " Leaderboard (page **"
             <> showt (pagenum + 1) <> "/" <> pack (show (length pages))
             <> "**):```\n" <> T.unlines (map (showLeaderboardRow lbt) $ pages !! pagenum) <> "```"

winsLeaderboardType :: LeaderboardType HypixelBowLeaderboardEntry Integer
winsLeaderboardType = LeaderboardType "Hypixel Bow Duels Wins" False "Wins" showt getHypixelBowLeaderboards (filterMaybe (>= 500) . bowLbWins)

lossesLeaderboardType :: LeaderboardType HypixelBowLeaderboardEntry Integer
lossesLeaderboardType = LeaderboardType "Hypixel Bow Duels Losses" False "Losses" showt getHypixelBowLeaderboards (fmap bowLbLosses . filterMaybe ((>= 500) . bowLbWins))

winstreakLeaderboardType :: LeaderboardType HypixelBowLeaderboardEntry Integer
winstreakLeaderboardType = LeaderboardType "Hypixel Bow Duels Winstreak" False "Winstreak" showt getHypixelBowLeaderboards (filterMaybe (>= 50) <=< bowLbWinstreak)

wlrLeaderboardType :: LeaderboardType HypixelBowLeaderboardEntry (WLR Integer)
wlrLeaderboardType = LeaderboardType "Hypixel Bow Duels WLR" False "WLR" showWLR getHypixelBowLeaderboards (fmap bowLbWLR . filterMaybe requirements)
  where
    requirements HypixelBowLeaderboardEntry {..} = bowLbWins >= bowLbLosses && bowLbWins >= 150

winsLeaderboardTypeGuild :: LeaderboardType HypixelBowLeaderboardEntry Integer
winsLeaderboardTypeGuild = LeaderboardType "Hypixel Bow Duels Wins" True "Wins" showt getHypixelBowLeaderboards (Just . bowLbWins)

lossesLeaderboardTypeGuild :: LeaderboardType HypixelBowLeaderboardEntry Integer
lossesLeaderboardTypeGuild = LeaderboardType "Hypixel Bow Duels Losses" True "Losses" showt getHypixelBowLeaderboards (Just . bowLbLosses)

winstreakLeaderboardTypeGuild :: LeaderboardType HypixelBowLeaderboardEntry Integer
winstreakLeaderboardTypeGuild = LeaderboardType "Hypixel Bow Duels Winstreak" True "Winstreak" showt getHypixelBowLeaderboards bowLbWinstreak

wlrLeaderboardTypeGuild :: LeaderboardType HypixelBowLeaderboardEntry (WLR Integer)
wlrLeaderboardTypeGuild = LeaderboardType "Hypixel Bow Duels WLR" True "WLR" showWLR getHypixelBowLeaderboards (Just . bowLbWLR)
