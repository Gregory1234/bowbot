{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.Hypixel.Announce where

import BowBot.BotData.Cached
import qualified Discord.Requests as R
import BowBot.Account.Basic
import qualified Data.HashMap.Strict as HM
import BowBot.Discord.Utils
import BowBot.BotData.Info
import BowBot.Discord.Account
import BowBot.Hypixel.TimeStats
import BowBot.Minecraft.Basic
import BowBot.Minecraft.Account
import Data.Bifunctor (first)
import qualified Data.Text as T

milestonesChannelInfo :: InfoType ChannelId
milestonesChannelInfo = InfoType { infoName = "milestones_channel", infoDefault = 0, infoParse = first pack . readEither . unpack }

milestoneNamesInfo :: InfoType [(Integer, Text)]
milestoneNamesInfo = InfoType { infoName = "division_title_milestones", infoDefault = [], infoParse = \s -> for (T.lines s) $ \l -> case T.splitOn "->" l of [a, b] -> (,b) <$> fmap fromInteger ((first pack . readEither . unpack) a); _ -> Left "wrong format" }

announceMilestones :: (MonadIOBotData m d r, Has DiscordHandle r, HasCaches [HypixelBowTimeStats 'DailyStats, BowBotAccount, DiscordAccount, InfoField, MinecraftAccount] d) => HM.HashMap UUID (HypixelBowTimeStats 'DailyStats) -> m ()
announceMilestones oldvals = do
  milestonesChannel <- askInfo milestonesChannelInfo
  milestoneNames <- askInfo milestoneNamesInfo
  curvals <- getCacheMap
  for_ (HM.toList curvals) $ \(uuid, cur) -> case oldvals HM.!? uuid of
    Nothing -> pure ()
    Just old -> when (cur /= old) $ for_ milestoneNames $ \(wins, name) -> when (bowTimeWins old < wins && bowTimeWins cur >= wins) $ do
      bbacc' <- getBowBotAccountByMinecraft uuid
      case bbacc' of
        Nothing -> pure ()
        Just BowBotAccount {..} -> do
          mcacc <- fromJust <$> getFromCache uuid
          dcaccounts <- getCacheMap
          let p = map (\x -> x { discordNickname = Nothing}) $ filter discordIsMember $ map (dcaccounts HM.!) accountDiscords
          unless (null p) $ do
            call_ $ R.CreateMessage milestonesChannel $ "**Congratulations** to **" <> head (mcNames mcacc) <> "** (" <> T.intercalate ", " (map showDiscordAccountDiscord p) <> ") for reaching " <> name <> "!"