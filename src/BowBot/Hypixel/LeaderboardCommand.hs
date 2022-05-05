{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module BowBot.Hypixel.LeaderboardCommand where

import BowBot.Command
import BowBot.Hypixel.Leaderboard
import BowBot.Minecraft.Arg
import BowBot.Minecraft.Account
import BowBot.Account.Basic
import Control.Monad.Except
import BowBot.Minecraft.Basic
import Discord.Types
import BowBot.BotData.Cached
import BowBot.Discord.Utils
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn, findIndex)
import Data.Maybe (mapMaybe, fromJust)
import Data.List.Split (chunksOf)
import Data.Functor (($>))
import Data.Either (isRight)

data LeaderboardType = LeaderboardType { leaderboardName :: String, leaderboardStatName :: String, leaderboardParser :: HypixelBowLeaderboardEntry -> Maybe (Integer, String) }

data LeaderboardResponse = LeaderboardPage Int | LeaderboardFind MinecraftResponseType String | LeaderboardAll

thePlayerIsntOnThisLeaderboardMessage :: String
thePlayerIsntOnThisLeaderboardMessage = "*The player isn't on this leaderboard!*"

throwUnlessAccountOnLeaderboard :: (HypixelBowLeaderboardEntry -> Maybe (Integer, String)) -> UUID -> ExceptT String CommandHandler ()
throwUnlessAccountOnLeaderboard leaderboardParser uuid = do
  lb <- liftMaybe "*The player hasn't had their stats checked or isn't on this leaderboard!*" =<< getFromCache uuid
  _ <- liftMaybe thePlayerIsntOnThisLeaderboardMessage (leaderboardParser lb)
  pure ()

filterAccountsOnLeaderboard :: (HypixelBowLeaderboardEntry -> Maybe (Integer, String)) -> [UUID] -> ExceptT String CommandHandler [UUID]
filterAccountsOnLeaderboard leaderboardParser = filterM $ fmap isRight . catchErrorEither . throwUnlessAccountOnLeaderboard leaderboardParser

leaderboardArgument :: (HypixelBowLeaderboardEntry -> Maybe (Integer, String)) -> Maybe String -> ExceptT String CommandHandler (LeaderboardResponse, [UUID])
leaderboardArgument _ (Just "all") = do
  acc <- lift (hEnv envSender) >>= getBowBotAccountByDiscord . userId
  return (LeaderboardAll, maybe [] accountMinecrafts acc)
leaderboardArgument _ (Just (readMaybe -> Just pagenum)) = do
  acc <- lift (hEnv envSender) >>= getBowBotAccountByDiscord . userId
  return (LeaderboardPage (pagenum - 1), maybe [] accountMinecrafts acc)
leaderboardArgument leaderboardParser Nothing = do
  acc <- lift (hEnv envSender) >>= getBowBotAccountByDiscord . userId
  onLb <- filterAccountsOnLeaderboard leaderboardParser (maybe [] accountMinecrafts acc)
  names <- getCacheMap
  if null onLb
    then return (LeaderboardPage 0, [])
    else return (LeaderboardFind JustResponse (head $ mcNames $ names HM.! accountSelectedMinecraft (fromJust acc)), onLb)
leaderboardArgument leaderboardParser (Just (fromPingDiscordUser -> Just did)) = do
  acc <- liftMaybe theUserIsntRegisteredMessage =<< getBowBotAccountByDiscord did
  onLb <- filterAccountsOnLeaderboard leaderboardParser (accountMinecrafts acc)
  names <- getCacheMap
  when (null onLb) $ throwError thePlayerIsntOnThisLeaderboardMessage
  return (LeaderboardFind JustResponse (head $ mcNames $ names HM.! accountSelectedMinecraft acc), onLb)
leaderboardArgument leaderboardParser (Just name) = do
  let helper MinecraftAccount {..} = throwUnlessAccountOnLeaderboard leaderboardParser mcUUID $> (True, ())
  MinecraftResponse {..} <- minecraftArgAutocorrect' helper name
  return (LeaderboardFind responseType (head $ mcNames responseAccount), [mcUUID responseAccount])

generateLeaderboardLines :: LeaderboardType -> [UUID] -> CommandHandler [(UUID, String)]
generateLeaderboardLines LeaderboardType {..} selected = do
  lb <- HM.toList <$> getCacheMap
  names <- getCacheMap
  return $ zipWith (\index (_, (uuid, str)) -> (uuid, pad 5 (show index ++ ".") ++ str)) [1 :: Integer ..] $ sortOn fst $ mapMaybe (\(uuid, lbe) -> (\(score, str) -> let MinecraftAccount {..} = names HM.! uuid in (-score, (mcUUID, (if mcUUID `elem` selected then "*" else " ") ++ pad 20 (head mcNames) ++ " ( " ++ str ++ " " ++ leaderboardStatName ++ " )"))) <$> leaderboardParser lbe) lb

leaderboardCommand :: LeaderboardType -> String -> Command
leaderboardCommand lbt@LeaderboardType {..} name = Command CommandInfo
  { commandName = name
  , commandHelpEntries =
    [ HelpEntry { helpUsage = name ++ " [name|page|\"all\"]", helpDescription = "show Bow Duels " ++ leaderboardName ++ " leaderboard", helpGroup = "normal" } ]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ hOneOptionalArgument (leaderboardArgument leaderboardParser) $ \case
    (LeaderboardPage pagenum, selected) -> do
      lb <- generateLeaderboardLines lbt selected
      let pages = chunksOf 20 (map snd lb)
      hRespond $ if pagenum < 0 || pagenum >= length pages
        then "*Wrong page number, it has to be between **1** and **" ++ show (length pages) ++ "**.*"
        else leaderboardName ++ " (page **" ++ show (pagenum + 1) ++ "/" ++ show (length pages) ++ "**):\n```\n" ++ unlines (pages !! pagenum) ++ "```"
    (LeaderboardFind rt nm, selected) -> do
      let didYouMean = case rt of
            JustResponse -> ""
            OldResponse _ -> ""
            DidYouMeanResponse -> "*Did you mean* **" ++ nm ++ "**:\n"
            DidYouMeanOldResponse o -> "*Did you mean* **" ++ o ++ "** (" ++ nm ++ "):"
      lb <- generateLeaderboardLines lbt selected
      let pages = chunksOf 20 lb
      let pagenum = fromJust $ findIndex (any ((`elem` selected) . fst)) pages
      hRespond $ leaderboardName ++ " (page **" ++ show (pagenum + 1) ++ "/" ++ show (length pages) ++ "**):\n" ++ didYouMean ++ "```\n" ++ unlines (map snd $ pages !! pagenum) ++ "```"
    (LeaderboardAll, selected) -> do
      lb <- generateLeaderboardLines lbt selected
      hRespondFile "lb.txt" $ unlines (map snd lb)

winsLeaderboardType :: LeaderboardType
winsLeaderboardType = LeaderboardType "Hypixel Bow Duels Wins Leaderboard" "Wins" $ \case
  HypixelBowLeaderboardEntry {..} | bowLbWins >= 500 -> Just (bowLbWins, show bowLbWins)
  _ -> Nothing

lossesLeaderboardType :: LeaderboardType
lossesLeaderboardType = LeaderboardType "Hypixel Bow Duels Losses Leaderboard" "Losses" $ \case
  HypixelBowLeaderboardEntry {..} | bowLbWins >= 500 -> Just (bowLbLosses, show bowLbLosses)
  _ -> Nothing

winstreakLeaderboardType :: LeaderboardType
winstreakLeaderboardType = LeaderboardType "Hypixel Bow Duels Winstreak Leaderboard" "Winstreak" $ \case
  HypixelBowLeaderboardEntry { bowLbWinstreak = (Just ws) } | ws >= 50 -> Just (ws, show ws)
  _ -> Nothing

wlrLeaderboardType :: LeaderboardType
wlrLeaderboardType = LeaderboardType "Hypixel Bow Duels WLR Leaderboard" "WLR" $ \case
  HypixelBowLeaderboardEntry {..} | bowLbWins >= bowLbLosses, bowLbWins >= 150 -> Just (if bowLbLosses == 0 then bowLbWins*100000000 else (bowLbWins*10000) `div` bowLbLosses, showWLR bowLbWins bowLbLosses)
  _ -> Nothing