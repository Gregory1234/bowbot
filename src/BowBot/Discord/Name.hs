module BowBot.Discord.Name where

import BowBot.Discord.Utils
import BowBot.DB.Basic
import Database.MySQL.Simple.QueryResults (QueryResults(..))
import Database.MySQL.Simple.QueryParams (QueryParams(..))
import qualified Data.Text as T

data DiscordName = DiscordName
  { discordUsername :: !Text
  , discordDiscrim :: !(Maybe Text)
  , discordNickname :: !(Maybe Text)
  } deriving (Show, Eq)

instance QueryParams DiscordName where
  renderParams DiscordName {..} = renderParams (discordUsername, fromMaybe "0" discordDiscrim, discordNickname)
instance QueryResults DiscordName where
  convertResults fields strings = let
    (discordUsername, filterMaybe (/="0") -> discordDiscrim, discordNickname) = convertResults fields strings
      in DiscordName {..}
instance QueryResultsSize DiscordName where
  queryResultsSize _ = 3

guildMemberToDiscordName :: GuildMember -> DiscordName
guildMemberToDiscordName GuildMember { memberUser = Just user, .. } = let u = userToDiscordName user in u { discordNickname = memberNick <|> discordNickname u }
guildMemberToDiscordName _ = DiscordName "" Nothing Nothing

userToDiscordName :: User -> DiscordName
userToDiscordName User {..} = DiscordName
  { discordUsername = fromMaybe userName $ filterMaybe nicknameOnlyCapitalization =<< userGlobalName
  , discordDiscrim = filterMaybe (/= "0") =<< userDiscrim
  , discordNickname = filterMaybe (not . nicknameOnlyCapitalization) =<< userGlobalName
  }
  where
    nicknameOnlyCapitalization = (== T.toLower userName) . T.toLower

showDiscordName :: DiscordName -> Text
showDiscordName DiscordName { discordNickname = Nothing, discordDiscrim = Just discrim, ..} = discordUsername <> "#" <> discrim
showDiscordName DiscordName { discordNickname = Nothing, discordDiscrim = Nothing, ..} = discordUsername
showDiscordName DiscordName { discordNickname = Just nick, discordDiscrim = Just discrim, ..} = nick <> " (" <> discordUsername <> "#" <> discrim <> ")"
showDiscordName DiscordName { discordNickname = Just nick, discordDiscrim = Nothing, ..} = nick <> " (" <> discordUsername <> ")"

showDiscordNameDiscord :: DiscordName -> Text
showDiscordNameDiscord DiscordName { discordNickname = Nothing, discordDiscrim = Just discrim, ..} = "**" <> discordEscape discordUsername <> "**#" <> discrim
showDiscordNameDiscord DiscordName { discordNickname = Nothing, discordDiscrim = Nothing, ..} = "**" <> discordEscape discordUsername <> "**"
showDiscordNameDiscord DiscordName { discordNickname = Just nick, discordDiscrim = Just discrim, ..} = "**" <> discordEscape nick <> "** (" <> discordEscape discordUsername <> "#" <> discrim <> ")"
showDiscordNameDiscord DiscordName { discordNickname = Just nick, discordDiscrim = Nothing, ..} = "**" <> discordEscape nick <> "** (" <> discordEscape discordUsername <> ")"
