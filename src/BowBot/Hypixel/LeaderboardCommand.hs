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
import BowBot.Hypixel.Guild
import qualified Data.Text as T
import Data.Bifunctor (second)
import BowBot.Command.Utils

data LeaderboardType = LeaderboardType
  { leaderboardName :: !Text
  , leaderboardIsGuild :: !Bool
  , leaderboardStatName :: !Text
  , leaderboardParser :: HypixelBowLeaderboardEntry -> Maybe (Integer, Text)
  }

leaderboardDescription :: LeaderboardType -> Text
leaderboardDescription LeaderboardType {..} = "show Bow Duels " <> leaderboardName <> (if leaderboardIsGuild then " guild" else "") <> " leaderboard"

thePlayerIsntOnThisLeaderboardMessage :: Text
thePlayerIsntOnThisLeaderboardMessage = "*The player isn't on this leaderboard!*"

data LeaderboardRow = LeaderboardRow
  { lbRowAccount :: MinecraftAccount
  , lbRowIndex :: Int
  , lbRowValue :: Integer
  , lbRowValueText :: Text
  , lbRowSelected :: Bool
  }

showLeaderboardRow :: Text -> LeaderboardRow -> Text
showLeaderboardRow name LeaderboardRow {..} = 
     pad 5 (showt lbRowIndex <> ".")
  <> (if lbRowSelected then "*" else " ")
  <> pad 20 (head (mcNames lbRowAccount))
  <> " ( " <> lbRowValueText <> " " <> name <> " )"

leaderboardCommand :: LeaderboardType -> Text -> Command
leaderboardCommand lbt@LeaderboardType {..} name = Command CommandInfo
  { commandName = name
  , commandHelpEntries =
    [ HelpEntry { helpUsage = name <> " [name|page|\"all\"]", helpDescription = leaderboardDescription lbt, helpGroup = "normal" } ]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument $ \case
    Just "all" -> do
      lb <- generateLeaderboardDiscordSelf
      respondFile (name <> ".txt") $ T.unlines (map (showLeaderboardRow leaderboardStatName) lb)
    Just (readMaybe @Int . unpack -> Just page) | page < 1000 -> do
      lb <- generateLeaderboardDiscordSelf
      displayLeaderboard (Just (page - 1)) "" lb
    Nothing -> do
      lb <- generateLeaderboardDiscordSelf
      displayLeaderboard Nothing "" lb
    Just (uuidFromString -> Just uuid) -> do
      acc <- liftMaybe thePlayerDoesNotExistMessage =<< getFromCache @MinecraftAccount uuid
      handlerMinecraft (autocorrectFromAccountDirect acc)
    Just (discordIdFromString -> Just did) -> do
      bbacc <- liftMaybe theUserIsntRegisteredMessage =<< getBowBotAccountByDiscord did
      lb <- generateLeaderboard (accountMinecrafts bbacc)
      displayLeaderboard Nothing "" lb
    Just n -> do
      ac <- liftMaybe thePlayerIsntOnThisLeaderboardMessage =<< minecraftAutocorrect n
      showSelfSkipTip (autocorrectAccount ac)
      handlerMinecraft ac
  where
    generateLeaderboardDiscordSelf :: ExceptT Text CommandHandler [LeaderboardRow]
    generateLeaderboardDiscordSelf = do
      did <- userId <$> envs envSender
      bbacc <- getBowBotAccountByDiscord did
      generateLeaderboard (maybe [] accountMinecrafts bbacc)
    generateLeaderboard :: [UUID] -> ExceptT Text CommandHandler [LeaderboardRow]
    generateLeaderboard selected = do
      lbFull <- HM.toList <$> getHypixelBowLeaderboards
      lb <- if leaderboardIsGuild then do
                gmems <- getHypixelGuildMembers
                return $ filter ((`elem` gmems) . fst) lbFull
              else return lbFull
      accs <- getCacheMap @MinecraftAccount
      let lbParsed = mapMaybe (sequence . second leaderboardParser) lb
      let lbSorted = sortOn (negate . fst . snd) lbParsed
      return $ zipWith (\lbRowIndex (uuid, (lbRowValue, lbRowValueText)) -> LeaderboardRow {lbRowAccount = accs HM.! uuid, lbRowSelected = uuid `elem` selected, ..}) [1..] lbSorted
    handlerMinecraft :: MinecraftAutocorrect -> ExceptT Text CommandHandler ()
    handlerMinecraft ac@MinecraftAutocorrect {..} = do
      lb <- generateLeaderboard [mcUUID autocorrectAccount]
      unless (any lbRowSelected lb) $ throwError thePlayerIsntOnThisLeaderboardMessage
      displayLeaderboard Nothing (if autocorrectIsDirect then "" else minecraftAutocorrectToHeader ac) lb
    displayLeaderboard :: Maybe Int -> Text -> [LeaderboardRow] -> ExceptT Text CommandHandler ()
    displayLeaderboard page headerExtra lb = do
      let pages = chunksOf 20 lb
      let pagenum = fromMaybe 0 $ page <|> findIndex (any lbRowSelected) pages
      when (pagenum < 0 || pagenum >= length pages) $ throwError $ "*Wrong page number, it has to be between **1** and **" <> showt (length pages) <> "**.*"
      respond $ headerExtra
             <> leaderboardName <> " Leaderboard (page **"
             <> showt (pagenum + 1) <> "/" <> pack (show (length pages))
             <> "**):```\n" <> T.unlines (map (showLeaderboardRow leaderboardStatName) $ pages !! pagenum) <> "```"

winsLeaderboardType :: LeaderboardType
winsLeaderboardType = LeaderboardType "Hypixel Bow Duels Wins" False "Wins" $ \case
  HypixelBowLeaderboardEntry {..} | bowLbWins >= 500 -> Just (bowLbWins, showt bowLbWins)
  _ -> Nothing

lossesLeaderboardType :: LeaderboardType
lossesLeaderboardType = LeaderboardType "Hypixel Bow Duels Losses" False "Losses" $ \case
  HypixelBowLeaderboardEntry {..} | bowLbWins >= 500 -> Just (bowLbLosses, showt bowLbLosses)
  _ -> Nothing

winstreakLeaderboardType :: LeaderboardType
winstreakLeaderboardType = LeaderboardType "Hypixel Bow Duels Winstreak" False "Winstreak" $ \case
  HypixelBowLeaderboardEntry { bowLbWinstreak = (Just ws) } | ws >= 50 -> Just (ws, showt ws)
  _ -> Nothing

wlrLeaderboardType :: LeaderboardType
wlrLeaderboardType = LeaderboardType "Hypixel Bow Duels WLR" False "WLR" $ \case
  HypixelBowLeaderboardEntry {..} | bowLbWins >= bowLbLosses, bowLbWins >= 150 -> Just (if bowLbLosses == 0 then bowLbWins*100000000 else (bowLbWins*10000) `div` bowLbLosses, showWLR bowLbWins bowLbLosses)
  _ -> Nothing

winsLeaderboardTypeGuild :: LeaderboardType
winsLeaderboardTypeGuild = LeaderboardType "Hypixel Bow Duels Wins" True "Wins" $ \HypixelBowLeaderboardEntry {..} -> Just (bowLbWins, showt bowLbWins)

lossesLeaderboardTypeGuild :: LeaderboardType
lossesLeaderboardTypeGuild = LeaderboardType "Hypixel Bow Duels Losses" True "Losses" $ \HypixelBowLeaderboardEntry {..} -> Just (bowLbLosses, showt bowLbLosses)

winstreakLeaderboardTypeGuild :: LeaderboardType
winstreakLeaderboardTypeGuild = LeaderboardType "Hypixel Bow Duels Winstreak" True "Winstreak" $ \HypixelBowLeaderboardEntry {..} -> fmap (\ws -> (ws, showt ws)) bowLbWinstreak

wlrLeaderboardTypeGuild :: LeaderboardType
wlrLeaderboardTypeGuild = LeaderboardType "Hypixel Bow Duels WLR" True "WLR" $ \HypixelBowLeaderboardEntry {..} -> Just (if bowLbLosses == 0 then bowLbWins*100000000 else (bowLbWins*10000) `div` bowLbLosses, showWLR bowLbWins bowLbLosses)