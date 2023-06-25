{-# LANGUAGE TypeFamilies #-}

module BowBot.Discord.Account (
  module BowBot.Discord.Account,
  module BowBot.Discord.Name
) where

import BowBot.Discord.Utils
import BowBot.DB.Basic
import BowBot.BotData.Info
import qualified Discord.Requests as R
import Data.List (deleteFirstsBy)
import BowBot.Discord.Name
import Data.Proxy

data DiscordAccount = DiscordAccount { discordId :: !UserId, discordName :: !DiscordName, discordIsMember :: !Bool } deriving (Show, Eq)

instance QueryParams DiscordAccount where
  renderParams DiscordAccount {..} = renderParams $ Concat (Only discordId, discordName, Only discordIsMember)
instance QueryResults DiscordAccount where
  convertResults fields strings = let
    Concat (Only discordId, discordName, Only discordIsMember) = convertResults fields strings
      in DiscordAccount {..}
instance QueryResultsSize DiscordAccount where
  queryResultsSize _ = 2 + queryResultsSize (Proxy @DiscordName)

getDiscordAccountById :: (MonadIOReader m r, Has Connection r) => UserId -> m (Maybe DiscordAccount)
getDiscordAccountById did = only <$> queryLog "SELECT `id`, `name`, `discriminator`, `nickname`, `member` FROM `discord` WHERE `id` = ?" (Only did)

getDiscordGuildMemberAccounts :: (MonadIOReader m r, Has Connection r) => m [DiscordAccount]
getDiscordGuildMemberAccounts = queryLog "SELECT `id`, `name`, `discriminator`, `nickname`, `member` FROM `discord` WHERE `member` = 1" ()

guildMemberToDiscordAccount :: GuildMember -> DiscordAccount
guildMemberToDiscordAccount gmem = DiscordAccount
  { discordId = maybe 0 userId $ memberUser gmem
  , discordName = guildMemberToDiscordName gmem
  , discordIsMember = True
  }

userToDiscordAccount :: User -> DiscordAccount
userToDiscordAccount user@User {..} = DiscordAccount
  { discordId = userId
  , discordName = userToDiscordName user
  , discordIsMember = False
  }

storeDiscordAccount :: (MonadIOReader m r, Has Connection r) => DiscordAccount -> m ()
storeDiscordAccount acc = void $ executeLog "INSERT INTO `discord` (`id`, `name`, `discriminator`, `nickname`, `member`) VALUES (?,?,?,?,?) ON DUPLICATE KEY UPDATE `name`=VALUES(`name`), `discriminator`=VALUES(`discriminator`), `nickname`=VALUES(`nickname`), `member`=VALUES(`member`)" acc

updateDiscordAccountCache :: (MonadIOReader m r, HasAll '[InfoCache, DiscordHandle, Connection] r) => m ()
updateDiscordAccountCache = do
  gid <- askInfo discordGuildIdInfo
  members <- map guildMemberToDiscordAccount . filter (\GuildMember {..} -> fmap userIsBot memberUser == Just False) <$> discordGuildMembers gid
  current :: [DiscordAccount] <- queryLog "SELECT `id`, `name`, `discriminator`, `nickname`, `member` FROM `discord`" ()
  updatedNonMembers <- for (deleteFirstsBy (\a b -> discordId a == discordId b) current members) $ \du -> do
    u' <- call $ R.GetUser (discordId du)
    case u' of
      Left e -> do
        logErrorFork $ showt e
        return du
      Right u -> return $ userToDiscordAccount u
  void $ executeManyLog "INSERT INTO `discord` (`id`, `name`, `discriminator`, `nickname`, `member`) VALUES (?,?,?,?,?) ON DUPLICATE KEY UPDATE `name`=VALUES(`name`), `discriminator`=VALUES(`discriminator`), `nickname`=VALUES(`nickname`), `member`=VALUES(`member`)" $ filter (`notElem` current) $ members ++ updatedNonMembers