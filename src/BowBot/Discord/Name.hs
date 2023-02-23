module BowBot.Discord.Name where

import BowBot.Discord.Utils
import BowBot.DB.Basic
import Database.MySQL.Simple.QueryResults (QueryResults(..))
import Database.MySQL.Simple.QueryParams (QueryParams(..))

data DiscordName = DiscordName
  { discordUsername :: !Text
  , discordDiscrim :: !Text
  , discordNickname :: !(Maybe Text)
  } deriving (Show, Eq)

instance QueryParams DiscordName where
  renderParams DiscordName {..} = renderParams (discordUsername, discordDiscrim, discordNickname)
instance QueryResults DiscordName where
  convertResults fields strings = let
    (discordUsername, discordDiscrim, discordNickname) = convertResults fields strings
      in DiscordName {..}
instance QueryResultsSize DiscordName where
  queryResultsSize _ = 3

guildMemberToDiscordName :: GuildMember -> DiscordName
guildMemberToDiscordName GuildMember { memberUser = Just user, .. } = (userToDiscordName user) { discordNickname = memberNick }
guildMemberToDiscordName _ = DiscordName "" "0000" Nothing

userToDiscordName :: User -> DiscordName
userToDiscordName User {..} = DiscordName
  { discordUsername = userName
  , discordDiscrim = fromMaybe "0000" userDiscrim
  , discordNickname = Nothing
  }

showDiscordName :: DiscordName -> Text
showDiscordName DiscordName { discordNickname = Nothing, ..} = discordUsername <> "#" <> discordDiscrim
showDiscordName DiscordName { discordNickname = Just nick, ..} = nick <> " (" <> discordUsername <> "#" <> discordDiscrim <> ")"

showDiscordNameDiscord :: DiscordName -> Text
showDiscordNameDiscord DiscordName { discordNickname = Nothing, ..} = "**" <> discordEscape discordUsername <> "**#" <> discordDiscrim
showDiscordNameDiscord DiscordName { discordNickname = Just nick, ..} = "**" <> discordEscape nick <> "** (" <> discordEscape discordUsername <> "#" <> discordDiscrim <> ")"
