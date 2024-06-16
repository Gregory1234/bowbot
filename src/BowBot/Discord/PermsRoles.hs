{-# LANGUAGE QuasiQuotes #-}

module BowBot.Discord.PermsRoles where

import BowBot.Perms.Basic
import BowBot.DB.Basic
import BowBot.Discord.Utils
import BowBot.BotData.Info
import qualified Data.Map as M
import Data.Bifunctor (first)

rankedModRoleInfo :: InfoType RoleId
rankedModRoleInfo = InfoType { infoName = "ranked_mod_role", infoDefault = 0, infoParse = first pack . readEither . unpack }

permsFromRoleIds :: (MonadIOReader m r, Has InfoCache r) => [RoleId] -> PermissionLevel -> m PermissionLevel
permsFromRoleIds roles level = do
  rankedModRole <- askInfo rankedModRoleInfo
  return $ case level of
    DefaultLevel | rankedModRole `elem` roles -> RankedModLevel
    RankedModLevel | rankedModRole `notElem` roles -> DefaultLevel
    _ -> level

-- TODO: update premissions immediately

updatePermsAll :: (MonadIOReader m r, HasAll '[SafeMysqlConn, DiscordHandle, InfoCache] r) => m ()
updatePermsAll = do
  permissions :: M.Map UserId PermissionLevel <- M.fromList <$> queryLog [mysql|SELECT `discord_id`, `level` FROM `permissions`|]
  gid <- askInfo discordGuildIdInfo
  members <- discordGuildMembers gid
  for_ members $ \GuildMember {..} -> case memberUser of
    Nothing -> return ()
    Just User {..} -> do
      let oldPerms = fromMaybe DefaultLevel $ permissions M.!? userId
      perms <- permsFromRoleIds memberRoles oldPerms
      when (perms /= oldPerms) $ 
        void $ executeLog [mysql|INSERT INTO `permissions`(`discord_id`, ^`level`) VALUES (userId, perms)|]

givePermsRoles :: (MonadIOReader m r, HasAll '[SafeMysqlConn, DiscordHandle, InfoCache] r) => GuildMember -> PermissionLevel -> m ()
givePermsRoles gmem perms = do
  gid <- askInfo discordGuildIdInfo
  rankedModRole <- askInfo rankedModRoleInfo
  addRemoveDiscordRoles gid gmem [rankedModRole] [rankedModRole | perms >= RankedModLevel]