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

rankedScoreReportChannelInfo :: InfoType ChannelId
rankedScoreReportChannelInfo = InfoType { infoName = "ranked_score_report_channel", infoDefault = 0, infoParse = first pack . readEither . unpack }

rankedModRoleInfo :: InfoType RoleId
rankedModRoleInfo = InfoType { infoName = "ranked_mod_role", infoDefault = 0, infoParse = first pack . readEither . unpack }

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
  rankedScoreReportChannel <- askInfo rankedScoreReportChannelInfo
  guard (messageChannelId == rankedScoreReportChannel)
  bid <- liftMaybe () =<< getBowBotIdByDiscord (userId messageAuthor)
  game <- liftMaybe () =<< getRankedGameByBowBotId bid
  let isFirstPlayer = fst (rankedPlayers game) == bid
  score <- liftMaybe () $ rankedBowScoreFromString isFirstPlayer messageContent
  guard =<< createRankedBowReport bid (rankedGameId game) messageId score
  for_ [positiveReaction, negativeReaction] $ addMessageReaction messageChannelId messageId

detectRankedBowReportReaction :: (MonadIOReader m r, HasAll '[DiscordHandle, SafeMysqlConn, InfoCache] r) => ReactionInfo -> m ()
detectRankedBowReportReaction ReactionInfo {..} = void $ runExceptT $ do
  val <- liftMaybe () $ reactionValue reactionEmoji
  rankedScoreReportChannel <- askInfo rankedScoreReportChannelInfo
  guard (reactionChannelId == rankedScoreReportChannel)
  report <- liftMaybe () =<< getRankedBowReportByMessageId reactionMessageId
  guard (reportStatus report == ReportActive)
  bid <- liftMaybe () =<< getBowBotIdByDiscord reactionUserId
  guard (reportAuthor report /= bid)
  game <- liftMaybe () =<< getRankedGameById (reportGameId report)
  guard (fst (rankedPlayers game) == bid || snd (rankedPlayers game) == bid)

  logInfo "Received a correct report reaction"
  liftIO $ threadDelay 5000000

  users <- getMessageReactions reactionChannelId reactionMessageId (emojiName reactionEmoji)
  guard (reactionUserId `elem` map userId users)
  report' <- liftMaybe () =<< getRankedBowReportByMessageId reactionMessageId
  guard (report == report')
  if val
    then do
      guard =<< applyEloByScore game (reportScore report)
      void $ finalizeRankedGame (rankedGameId game)
    else do
      guard =<< setRankedBowReportStatusByMessageId reactionMessageId ReportRejected
      rankedModRole <- askInfo rankedModRoleInfo
      call_ $ R.CreateMessage reactionChannelId $ "<@&" <> showt rankedModRole <> ">"
