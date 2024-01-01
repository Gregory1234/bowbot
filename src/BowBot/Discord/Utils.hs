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
  members <- call $ R.ListGuildMembers gid R.GuildMembersTiming { R.guildMembersTimingLimit = Just 1000, R.guildMembersTimingAfter = Nothing } -- TODO: what if there are more?
  case members of
    Left e -> do
      logErrorFork $ showt e
      return []
    Right m -> return (filter (maybe False (not . userIsBot) . memberUser) m)

getDiscordGuildMember :: (MonadIOReader m r, Has DiscordHandle r) => GuildId -> UserId -> m (Maybe GuildMember)
getDiscordGuildMember gid uid = do
  member <- call $ R.GetGuildMember gid uid
  case member of
    Left e -> do
      logErrorFork $ showt e
      return Nothing
    Right m -> return $ if maybe True userIsBot (memberUser m) then Nothing else Just m

addRemoveDiscordRoles :: (MonadIOReader m r, Has DiscordHandle r) => GuildId -> GuildMember -> [RoleId] -> [RoleId] -> m ()
addRemoveDiscordRoles gid GuildMember {..} universe correct = do
  let current = memberRoles `intersect` universe
  let toAdd = correct \\ current
  let toRemove = current \\ correct
  let did = maybe 0 userId memberUser
  for_ toAdd $ \r -> call_ $ R.AddGuildMemberRole gid did r
  for_ toRemove $ \r -> call_ $ R.RemoveGuildMemberRole gid did r

discordFormatTimestamp :: Maybe Text -> UTCTime -> Text
discordFormatTimestamp style timestamp = "<t:" <> showt (timestampToUnixSecond timestamp) <> maybe "" (T.cons ':') style <> ">"

discordFormatTimestampFull :: UTCTime -> Text
discordFormatTimestampFull time = discordFormatTimestamp (Just "R") time <> " (" <> discordFormatTimestamp Nothing time <> ")"

discordIdFromPing :: Text -> Maybe UserId
discordIdFromPing str | "<@" `T.isPrefixOf` str && ">" `T.isSuffixOf` str = readMaybe $ unpack $ T.filter isDigit str
discordIdFromPing _ = Nothing

discordIdFromString :: Text -> Maybe UserId
discordIdFromString t = (readMaybe @UserId $ unpack t) <|> discordIdFromPing t