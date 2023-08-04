module BowBot.Discord.Name where

import BowBot.Discord.Utils
import BowBot.DB.Basic
import qualified Data.Text as T

data DiscordName = DiscordName
  { discordUsername :: !Text
  , discordDiscrim :: !(Maybe Text)
  , discordNickname :: !(Maybe Text)
  } deriving (Show, Eq)

instance ToMysql DiscordName where
  toActions DiscordName {..} = toActions discordUsername ++ toActions (fromMaybe "0" discordDiscrim) ++ toActions discordNickname
instance FromMysql DiscordName where
  rowParser = DiscordName <$> rowParser <*> (filterMaybe (/= "0") <$> rowParser) <*> rowParser

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
