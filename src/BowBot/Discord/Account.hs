{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module BowBot.Discord.Account where

import BowBot.BotData.Cached
import BowBot.Discord.Utils
import qualified Data.HashMap.Strict as HM
import BowBot.DB.Basic
import BowBot.BotData.Info
import qualified Discord.Requests as R
import Data.List (deleteFirstsBy)

data DiscordAccount = DiscordAccount { discordId :: !UserId, discordName :: !Text, discordDiscrim :: !Text, discordNickname :: !(Maybe Text), discordIsMember :: !Bool } deriving (Show, Eq)

instance Cached DiscordAccount where
  type CacheIndex DiscordAccount = UserId
  refreshCache = do
    cache <- getCache
    res :: [(UserId, Text, Text, Maybe Text, Bool)] <- queryLog "SELECT `id`, `name`, `discriminator`, `nickname`, `member` FROM `discord`" ()
    let newValues = HM.fromList $ flip fmap res $ \(discordId, discordName, discordDiscrim, discordNickname, discordIsMember) -> (discordId, DiscordAccount {..})
    liftIO $ atomically $ writeTVar cache newValues

instance CachedIndexed DiscordAccount where
  cacheIndex = discordId
  storeInCache accs = do
    cacheMap <- getCacheMap
    let toQueryParams acc@DiscordAccount {..} = if Just acc == cacheMap HM.!? discordId then Nothing else Just (discordId, discordName, discordDiscrim, discordNickname, discordIsMember)
    let queryParams = mapMaybe toQueryParams accs
    success <- liftIO $ withDB $ \conn -> (>0) <$> executeManyLog' conn "INSERT INTO `discord` (`id`, `name`, `discriminator`, `nickname`, `member`) VALUES (?,?,?,?,?) ON DUPLICATE KEY UPDATE `name`=VALUES(`name`), `discriminator`=VALUES(`discriminator`), `nickname`=VALUES(`nickname`), `member`=VALUES(`member`)" queryParams
    when success $ do
      cache <- getCache
      liftIO $ atomically $ modifyTVar cache (insertMany (map (\x -> (discordId x, x)) accs))
    return success

guildMemberToDiscordAccount :: GuildMember -> DiscordAccount
guildMemberToDiscordAccount GuildMember { memberUser = Just user, .. } = (userToDiscordAccount user) { discordNickname = memberNick, discordIsMember = True }
guildMemberToDiscordAccount _ = DiscordAccount 0 "" "0000" Nothing True

userToDiscordAccount :: User -> DiscordAccount
userToDiscordAccount User {..} = DiscordAccount
  { discordId = userId
  , discordName = userName
  , discordDiscrim = fromMaybe "0000" userDiscrim
  , discordNickname = Nothing
  , discordIsMember = False
  }

showDiscordAccount :: DiscordAccount -> Text
showDiscordAccount DiscordAccount { discordNickname = Nothing, ..} = discordName <> "#" <> discordDiscrim
showDiscordAccount DiscordAccount { discordNickname = Just nick, ..} = nick <> " (" <> discordName <> "#" <> discordDiscrim <> ")"

showDiscordAccountDiscord :: DiscordAccount -> Text
showDiscordAccountDiscord DiscordAccount { discordNickname = Nothing, ..} = "**" <> discordEscape discordName <> "**#" <> discordDiscrim
showDiscordAccountDiscord DiscordAccount { discordNickname = Just nick, ..} = "**" <> discordEscape nick <> "** (" <> discordEscape discordName <> "#" <> discordDiscrim <> ")"

updateDiscordAccountCache :: (MonadIOBotData m d r, Has DiscordHandle r, HasCaches [InfoField, DiscordAccount] d) => m ()
updateDiscordAccountCache = do
  gid <- askInfo discordGuildIdInfo
  members <- map guildMemberToDiscordAccount . filter (\GuildMember {..} -> fmap userIsBot memberUser == Just False) <$> discordGuildMembers gid
  current <- HM.elems <$> getCacheMap
  updatedNonMembers <- for (deleteFirstsBy (\a b -> discordId a == discordId b) current members) $ \du -> do
    u' <- call $ R.GetUser (discordId du)
    case u' of
      Left e -> do
        logErrorFork $ showt e
        return du
      Right u -> return $ userToDiscordAccount u
  void $ storeInCache $ members ++ updatedNonMembers