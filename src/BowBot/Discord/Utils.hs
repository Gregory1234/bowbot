module BowBot.Discord.Utils(
  module BowBot.Discord.Utils, module BowBot.Discord.Class, module Discord, module Discord.Types, module BowBot.Utils
) where

import BowBot.Discord.DiscordNFData ()
import BowBot.Discord.Class
import Discord
import Discord.Types
import qualified Discord.Requests as R
import BowBot.Utils
import BowBot.DB.Basic
import Data.List (isPrefixOf, isSuffixOf)
import Data.Char (isDigit)

discordGuildMembers :: MonadDiscord m => GuildId -> m [GuildMember]
discordGuildMembers gid = do
  members <- call $ R.ListGuildMembers gid R.GuildMembersTiming { R.guildMembersTimingLimit = Just 500, R.guildMembersTimingAfter = Nothing } -- TODO: what if there are more?
  case members of
    Left e -> do
      logError (show e)
      return []
    Right m -> return m

fromPingDiscordUser :: String -> Maybe UserId
fromPingDiscordUser str | "<@" `isPrefixOf` str && ">" `isSuffixOf` str = readMaybe $ filter isDigit str
fromPingDiscordUser _ = Nothing