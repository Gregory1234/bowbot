{-# LANGUAGE TypeFamilies #-}

module BowBot.Discord.Account (
  module BowBot.Discord.Account,
  module BowBot.Discord.Name
) where

import BowBot.Discord.Utils
import BowBot.DB.Typed
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
instance DatabaseTable DiscordAccount where
  type PrimaryKey DiscordAccount = Only UserId
  databaseTableName _ = "discord"
  databaseColumnNames _ = ["id", "name", "discriminator", "nickname", "member"]
  databasePrimaryKey _ = ["id"]

getDiscordAccountById :: (MonadIOReader m r, Has Connection r) => UserId -> m (Maybe DiscordAccount)
getDiscordAccountById did = queryOnlyLogT selectByPrimaryQuery (Only did)

getDiscordGuildMemberAccounts :: (MonadIOReader m r, Has Connection r) => m [DiscordAccount]
getDiscordGuildMemberAccounts = queryLogT_ (selectQueryWithSuffix " WHERE `member` = 1")

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
storeDiscordAccount acc = void $ executeLogT insertQuery acc

updateDiscordAccountCache :: (MonadIOReader m r, HasAll '[InfoCache, DiscordHandle, Connection] r) => m ()
updateDiscordAccountCache = do
  gid <- askInfo discordGuildIdInfo
  members <- map guildMemberToDiscordAccount . filter (\GuildMember {..} -> fmap userIsBot memberUser == Just False) <$> discordGuildMembers gid
  current :: [DiscordAccount] <- queryLogT_ selectAllQuery
  updatedNonMembers <- for (deleteFirstsBy (\a b -> discordId a == discordId b) current members) $ \du -> do
    u' <- call $ R.GetUser (discordId du)
    case u' of
      Left e -> do
        logErrorFork $ showt e
        return du
      Right u -> return $ userToDiscordAccount u
  void $ executeManyLogT insertQuery $ filter (`notElem` current) $ members ++ updatedNonMembers