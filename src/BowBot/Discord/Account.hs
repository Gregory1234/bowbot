{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

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

data DiscordAccount = DiscordAccount { discordId :: !UserId, discordName :: !DiscordName, discordIsMember :: !Bool }
  deriving stock (Show, Eq, Generic)
  deriving (ToMysql, FromMysql) via (Generically DiscordAccount)


$(pure [])

getDiscordAccountById :: (MonadIOReader m r, Has Connection r) => UserId -> m (Maybe DiscordAccount)
getDiscordAccountById did = queryOnlyLogT [mysql|SELECT DiscordAccount FROM `discord` WHERE `id` = did|]

getDiscordGuildMemberAccounts :: (MonadIOReader m r, Has Connection r) => m [DiscordAccount]
getDiscordGuildMemberAccounts = queryLogT [mysql|SELECT DiscordAccount FROM `discord` WHERE `member`|]

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
storeDiscordAccount acc = void $ executeLogT [mysql|INSERT INTO `discord`(DiscordAccount) VALUES acc|]

updateDiscordAccountCache :: (MonadIOReader m r, HasAll '[InfoCache, DiscordHandle, Connection] r) => m ()
updateDiscordAccountCache = do
  gid <- askInfo discordGuildIdInfo
  members <- map guildMemberToDiscordAccount . filter (\GuildMember {..} -> fmap userIsBot memberUser == Just False) <$> discordGuildMembers gid
  current :: [DiscordAccount] <- queryLogT [mysql|SELECT DiscordAccount FROM `discord`|]
  updatedNonMembers <- for (deleteFirstsBy (\a b -> discordId a == discordId b) current members) $ \du -> do
    u' <- call $ R.GetUser (discordId du)
    case u' of
      Left e -> do
        logErrorFork $ showt e
        return du
      Right u -> return $ userToDiscordAccount u
  let toInsert = filter (`notElem` current) $ members ++ updatedNonMembers
  void $ executeLogT [mysql|INSERT INTO `discord`(DiscordAccount) VALUES toInsert..|] 