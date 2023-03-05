module BowBot.Hypixel.LeaderboardCommand where

import BowBot.Command
import BowBot.Hypixel.Leaderboard
import BowBot.Minecraft.Arg
import BowBot.Minecraft.Account
import BowBot.Account.Basic
import BowBot.Account.Arg
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

data LeaderboardResponse = LeaderboardPage Int | LeaderboardFind MinecraftResponseTime MinecraftResponseAutocorrect Text | LeaderboardAll

thePlayerIsntOnThisLeaderboardMessage :: Text
thePlayerIsntOnThisLeaderboardMessage = "*The player isn't on this leaderboard!*"

throwUnlessAccountOnLeaderboard :: (HypixelBowLeaderboardEntry -> Maybe (Integer, Text)) -> UUID -> ExceptT Text CommandHandler ()
throwUnlessAccountOnLeaderboard leaderboardParser uuid = do
  lb <- liftMaybe "*The player hasn't had their stats checked or isn't on this leaderboard!*" =<< getHypixelBowLeaderboardEntryByUUID uuid -- TODO: don't query the database again
  _ <- liftMaybe thePlayerIsntOnThisLeaderboardMessage (leaderboardParser lb)
  pure ()

filterAccountsOnLeaderboard :: (HypixelBowLeaderboardEntry -> Maybe (Integer, Text)) -> [UUID] -> ExceptT Text CommandHandler [UUID]
filterAccountsOnLeaderboard leaderboardParser = filterM $ fmap isRight . catchErrorEither . throwUnlessAccountOnLeaderboard leaderboardParser

leaderboardArgument :: (HypixelBowLeaderboardEntry -> Maybe (Integer, Text)) -> Maybe Text -> ExceptT Text CommandHandler (LeaderboardResponse, [UUID])
leaderboardArgument _ (Just "all") = do
  acc <- lift (envs envSender) >>= getBowBotAccountByDiscord . userId
  return (LeaderboardAll, maybe [] accountMinecrafts acc)
leaderboardArgument _ (Just (readMaybe . unpack -> Just pagenum)) = do -- TODO: big numbers could be discord ids...
  acc <- lift (envs envSender) >>= getBowBotAccountByDiscord . userId
  return (LeaderboardPage (pagenum - 1), maybe [] accountMinecrafts acc)
leaderboardArgument leaderboardParser maybename = do
  names <- getCacheMap
  mcOrDc <- lift (envs envSender) >>= flip noAccountArgFull maybename . userId
  case (maybename, mcOrDc) of
    (Nothing, AccountDiscordResponse dc) -> do
      acc <- getBowBotAccountByDiscord (discordId dc)
      onLb <- filterAccountsOnLeaderboard leaderboardParser (maybe [] accountMinecrafts acc)
      if null onLb
        then return (LeaderboardPage 0, [])
        else return (LeaderboardFind CurrentResponse ResponseTrue (head $ mcNames $ names HM.! accountSelectedMinecraft (fromJust acc)), onLb)
    (Just _, AccountDiscordResponse dc) -> do
      acc <- liftMaybe theUserIsntRegisteredMessage =<< getBowBotAccountByDiscord (discordId dc)
      onLb <- filterAccountsOnLeaderboard leaderboardParser (accountMinecrafts acc)
      when (null onLb) $ throwError thePlayerIsntOnThisLeaderboardMessage
      return (LeaderboardFind CurrentResponse ResponseTrue (head $ mcNames $ names HM.! accountSelectedMinecraft acc), onLb)
    (_, AccountMinecraftResponse MinecraftResponse {..}) -> do
      throwUnlessAccountOnLeaderboard leaderboardParser (mcUUID mcResponseAccount)
      return (LeaderboardFind mcResponseTime mcResponseAutocorrect (head $ mcNames mcResponseAccount), [mcUUID mcResponseAccount])

generateLeaderboardLines :: LeaderboardType -> [UUID] -> ExceptT Text CommandHandler [(UUID, Text)]
generateLeaderboardLines LeaderboardType {..} selected = do
  lb <- HM.toList <$> getHypixelBowLeaderboards
  names <- getCacheMap
  return $ zipWith (\index (_, (uuid, str)) -> (uuid, pad 5 (showt index <> ".") <> str)) [1 :: Integer ..] $ sortOn fst $ mapMaybe (\(uuid, lbe) -> (\(score, str) -> let MinecraftAccount {..} = names HM.! uuid in (-score, (mcUUID, (if mcUUID `elem` selected then "*" else " ") <> pad 20 (head mcNames) <> " ( " <> str <> " " <> leaderboardStatName <> " )"))) <$> leaderboardParser lbe) lb

leaderboardCommand :: LeaderboardType -> Text -> Command
leaderboardCommand lbt@LeaderboardType {..} name = Command CommandInfo
  { commandName = name
  , commandHelpEntries =
    [ HelpEntry { helpUsage = name <> " [name|page|\"all\"]", helpDescription = "show Bow Duels " <> leaderboardName <> " leaderboard", helpGroup = "normal" } ]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument $ leaderboardArgument leaderboardParser >=> \case
    (LeaderboardPage pagenum, selected) -> do
      lb <- generateLeaderboardLines lbt selected
      let pages = chunksOf 20 (map snd lb)
      respond $ if pagenum < 0 || pagenum >= length pages
        then "*Wrong page number, it has to be between **1** and **" <> showt (length pages) <> "**.*"
        else leaderboardName <> " Leaderboard (page **" <> showt (pagenum + 1) <> "/" <> pack (show (length pages)) <> "**):\n```\n" <> T.unlines (pages !! pagenum) <> "```"
    (LeaderboardFind rt ra nm, selected) -> do
      let didYouMean = case (rt, ra) of
            (CurrentResponse, ResponseAutocorrect) -> "*Did you mean* **" <> discordEscape nm <> "**:\n"
            (OldResponse o, ResponseAutocorrect) -> "*Did you mean* **" <> discordEscape o <> "** (" <> discordEscape nm <> "):"
            (_, _) -> ""
      lb <- generateLeaderboardLines lbt selected
      let pages = chunksOf 20 lb
      let pagenum = fromJust $ findIndex (any ((`elem` selected) . fst)) pages
      respond $ leaderboardName <> " Leaderboard (page **" <> showt (pagenum + 1) <> "/" <> pack (show (length pages)) <> "**):\n" <> didYouMean <> "```\n" <> T.unlines (map snd $ pages !! pagenum) <> "```"
    (LeaderboardAll, selected) -> do
      lb <- generateLeaderboardLines lbt selected
      respondFile "lb.txt" $ T.unlines (map snd lb)

-- TODO: refactor all of this!!!

leaderboardArgumentGuild :: (HypixelBowLeaderboardEntry -> Maybe (Integer, Text)) -> Maybe Text -> ExceptT Text CommandHandler ([UUID], LeaderboardResponse, [UUID])
leaderboardArgumentGuild parser arg = do
  (res, sel) <- leaderboardArgument parser arg
  gmems <- getHypixelGuildMembers
  case res of
    LeaderboardFind {} | all (`notElem` gmems) sel -> case arg of
      Nothing -> return (gmems, LeaderboardPage 0, [])
      Just _ -> throwError thePlayerIsntOnThisLeaderboardMessage
    _ -> return (gmems, res, filter (`elem` gmems) sel)

generateLeaderboardLinesGuild :: LeaderboardType -> [UUID] -> [UUID] -> ExceptT Text CommandHandler [(UUID, Text)]
generateLeaderboardLinesGuild LeaderboardType {..} selected gmems = do
  lb <- HM.toList . HM.filterWithKey (\k _ -> k `elem` gmems) <$> getHypixelBowLeaderboards
  names <- getCacheMap
  return $ zipWith (\index (_, (uuid, str)) -> (uuid, pad 5 (showt index <> ".") <> str)) [1 :: Integer ..] $ sortOn fst $ mapMaybe (\(uuid, lbe) -> (\(score, str) -> let MinecraftAccount {..} = names HM.! uuid in (-score, (mcUUID, (if mcUUID `elem` selected then "*" else " ") <> pad 20 (head mcNames) <> " ( " <> str <> " " <> leaderboardStatName <> " )"))) <$> leaderboardParser lbe) lb


leaderboardGuildCommand :: LeaderboardType -> Text -> Command
leaderboardGuildCommand lbt@LeaderboardType {..} name = Command CommandInfo
  { commandName = name
  , commandHelpEntries =
    [ HelpEntry { helpUsage = name <> " [name|page|\"all\"]", helpDescription = "show Bow Duels " <> leaderboardName <> " guild leaderboard", helpGroup = "normal" } ]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument $ leaderboardArgumentGuild leaderboardParser >=> \case
    (gmems, LeaderboardPage pagenum, selected) -> do
      lb <- generateLeaderboardLinesGuild lbt selected gmems
      let pages = chunksOf 20 (map snd lb)
      respond $ if pagenum < 0 || pagenum >= length pages
        then "*Wrong page number, it has to be between **1** and **" <> showt (length pages) <> "**.*"
        else leaderboardName <> " Guild Leaderboard (page **" <> showt (pagenum + 1) <> "/" <> pack (show (length pages)) <> "**):\n```\n" <> T.unlines (pages !! pagenum) <> "```"
    (gmems, LeaderboardFind rt ra nm, selected) -> do
      let didYouMean = case (rt, ra) of
            (CurrentResponse, ResponseAutocorrect) -> "*Did you mean* **" <> discordEscape nm <> "**:\n"
            (OldResponse o, ResponseAutocorrect) -> "*Did you mean* **" <> discordEscape o <> "** (" <> discordEscape nm <> "):"
            (_, _) -> ""
      lb <- generateLeaderboardLinesGuild lbt selected gmems
      let pages = chunksOf 20 lb
      let pagenum = fromJust $ findIndex (any ((`elem` selected) . fst)) pages
      respond $ leaderboardName <> " Guild Leaderboard (page **" <> showt (pagenum + 1) <> "/" <> pack (show (length pages)) <> "**):\n" <> didYouMean <> "```\n" <> T.unlines (map snd $ pages !! pagenum) <> "```"
    (gmems, LeaderboardAll, selected) -> do
      lb <- generateLeaderboardLinesGuild lbt selected gmems
      respondFile "lbg.txt" $ T.unlines (map snd lb)

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