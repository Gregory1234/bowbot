{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.Discord.Utils(
  module BowBot.Discord.Utils, module BowBot.Discord.Basic, module Discord, module Discord.Types, module BowBot.Utils
) where

import BowBot.Discord.Orphans ()
import BowBot.Discord.Basic
import Discord
import Discord.Types
import qualified Discord.Requests as R
import BowBot.Utils
import BowBot.DB.Basic
import qualified Data.Text as T

discordGuildMembers :: (MonadIOReader m r, Has DiscordHandle r) => GuildId -> m [GuildMember]
discordGuildMembers gid = do
  members <- call $ R.ListGuildMembers gid R.GuildMembersTiming { R.guildMembersTimingLimit = Just 500, R.guildMembersTimingAfter = Nothing } -- TODO: what if there are more?
  case members of
    Left e -> do
      logErrorFork $ showt e
      return []
    Right m -> return (filter (maybe False (not . userIsBot) . memberUser) m)

fromPingDiscordUser :: Text -> Maybe UserId
fromPingDiscordUser str | "<@" `T.isPrefixOf` str && ">" `T.isSuffixOf` str = readMaybe $ unpack $ T.filter isDigit str
fromPingDiscordUser _ = Nothing

discordFormatTimestamp :: Maybe Text -> UTCTime -> Text
discordFormatTimestamp style timestamp = "<t:" <> showt (timestampToUnixSecond timestamp) <> maybe "" (T.cons ':') style <> ">"

discordFormatTimestampFull :: UTCTime -> Text
discordFormatTimestampFull time = discordFormatTimestamp (Just "R") time <> " (" <> discordFormatTimestamp Nothing time <> ")"