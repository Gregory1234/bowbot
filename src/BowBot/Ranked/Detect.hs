module BowBot.Ranked.Detect where

import BowBot.Discord.Utils
import BowBot.DB.Basic
import BowBot.BotData.Info
import Control.Monad.Except (runExceptT)
import Data.Bifunctor (first)
import BowBot.Ranked.Game
import BowBot.Account.Basic
import BowBot.Ranked.Report
import Control.Concurrent
import qualified Discord.Requests as R
import BowBot.Ranked.EloUpdate
import BowBot.Ranked.Queue
import qualified Data.Text as T

rankedScoreReportChannelInfo :: InfoType ChannelId
rankedScoreReportChannelInfo = InfoType { infoName = "ranked_score_report_channel", infoDefault = 0, infoParse = first pack . readEither . unpack }

positiveReaction, negativeReaction :: Text
positiveReaction = "✅"
negativeReaction = "❌"

reactionValue :: Emoji -> Maybe Bool
reactionValue Emoji { emojiName = ((==positiveReaction) -> True) } = Just True
reactionValue Emoji { emojiName = ((==negativeReaction) -> True) } = Just False
reactionValue _ = Nothing

detectRankedBowReport :: (MonadIOReader m r, HasAll '[DiscordHandle, SafeMysqlConn, InfoCache] r) => Message -> m ()
detectRankedBowReport Message {..} = void $ runExceptT $ do
  guard (not $ userIsBot messageAuthor)
  let msgParts = T.words messageContent
  guard (length msgParts == 2)
  rankedScoreReportChannel <- askInfo rankedScoreReportChannelInfo
  guard (messageChannelId == rankedScoreReportChannel)
  bid <- liftMaybe () =<< getBowBotIdByDiscord (userId messageAuthor)
  game <- liftMaybe () =<< getRankedGameByBowBotId bid
  let isFirstPlayer = fst (rankedPlayers game) == bid
  let otherPlayer = (if isFirstPlayer then snd else fst) (rankedPlayers game)
  otherPlayerDiscords <- getDiscordIdsByBowBotId otherPlayer
  guard $ discordIdFromPing (msgParts !! 1) `elem` map Just otherPlayerDiscords
  score <- liftMaybe () $ rankedBowScoreFromString isFirstPlayer (msgParts !! 0)
  guard =<< createRankedBowReport bid (rankedGameId game) messageId score
  for_ [positiveReaction, negativeReaction] $ addMessageReaction messageChannelId messageId

detectRankedBowReportReaction :: (MonadIOReader m r, HasAll '[DiscordHandle, SafeMysqlConn, InfoCache] r) => ReactionInfo -> m ()
detectRankedBowReportReaction ReactionInfo {..} = void $ runExceptT $ do
  val <- liftMaybe () $ reactionValue reactionEmoji
  rankedScoreReportChannel <- askInfo rankedScoreReportChannelInfo
  guard (reactionChannelId == rankedScoreReportChannel)
  report <- liftMaybe () =<< getRankedBowReportByMessageId reactionMessageId
  game <- liftMaybe () =<< getRankedGameById (reportGameId report)
  bid <- liftMaybe () =<< getBowBotIdByDiscord reactionUserId
  if fst (rankedPlayers game) == bid || snd (rankedPlayers game) == bid
    then do
      guard (reportAuthor report /= bid)
      guard (reportStatus report == ReportActive)
    else do
      guard val
      modRole <- askInfo rankedModRoleInfo
      guildId <- askInfo discordGuildIdInfo
      guildMember <- liftMaybe () =<< getDiscordGuildMember guildId reactionUserId
      guard (modRole `elem` memberRoles guildMember)

  logInfo "Received a correct report reaction"
  liftIO $ threadDelay 5000000

  users <- getMessageReactions reactionChannelId reactionMessageId (emojiName reactionEmoji)
  guard (reactionUserId `elem` map userId users)
  report' <- liftMaybe () =<< getRankedBowReportByMessageId reactionMessageId
  guard (report == report')
  if val
    then do
      updates <- liftMaybe () =<< applyEloByScore game (reportScore report)
      guard =<< finalizeRankedGame GameCompleted (rankedGameId game)
      guard =<< announceEloUpdate (rankedGameQueue game) (Just (True, rankedGameId game)) updates
      guard =<< setRankedGameScore (rankedGameId game) (Just (reportScore report))
    else do
      guard =<< setRankedBowReportStatusByMessageId reactionMessageId ReportRejected
      rankedModRole <- askInfo rankedModRoleInfo
      call_ $ R.CreateMessage reactionChannelId $ "<@&" <> showt rankedModRole <> "> **A self report rejected by the other player in Game #" <> showt (reportGameId report) <> "!**"
