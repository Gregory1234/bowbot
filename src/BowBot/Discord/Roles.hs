{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.Discord.Roles where

import qualified Data.Map as M
import BowBot.Hypixel.Leaderboard
import Discord.Types
import BowBot.BotData.Info
import Data.List.Split (splitOn)
import BowBot.Discord.Utils
import BowBot.BotData.Cached
import BowBot.BotData.CachedSingle
import BowBot.BotData.Counter
import BowBot.Account.Basic
import Data.Proxy
import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe)
import qualified Discord.Requests as R
import Data.List ((\\), intersect)
import BowBot.DB.Basic (queryLog)
import BowBot.Hypixel.Guild
import BowBot.Network.Class (MonadNetwork)
import BowBot.Hypixel.Basic



divisionTitleRolesInfo :: InfoType [(Integer, RoleId)]
divisionTitleRolesInfo = InfoType { infoName = "division_title_roles", infoDefault = [], infoParse = \s -> for (lines s) $ \l -> case splitOn "->" l of [a, b] -> (,) <$> readEither a <*> fmap fromInteger (readEither b); _ -> Left "wrong format" }

toggleableRolesInfo :: InfoType (M.Map String RoleId)
toggleableRolesInfo = InfoType { infoName = "toggleable_roles", infoDefault = M.empty, infoParse = \s -> fmap M.fromList $ for (lines s) $ \l -> case splitOn "->" l of [a, b] -> (a,) <$> fmap fromInteger (readEither b); _ -> Left "wrong format" }

savedRolesInfo :: InfoType (M.Map String RoleId)
savedRolesInfo = InfoType { infoName = "saved_roles", infoDefault = M.empty, infoParse = \s -> fmap M.fromList $ for (lines s) $ \l -> case splitOn "->" l of [a, b] -> (a,) <$> fmap fromInteger (readEither b); _ -> Left "wrong format" }

savedHypixelRolesInfo :: InfoType (M.Map String ([String], RoleId))
savedHypixelRolesInfo = InfoType { infoName = "hypixel_roles", infoDefault = M.empty, infoParse = \s -> fmap M.fromList $ for (lines s) $ \l -> case splitOn "->" l of [a, b, c] -> (a,) . (splitOn "|" b,) <$> fmap fromInteger (readEither c); _ -> Left "wrong format" }

illegalRoleInfo :: InfoType RoleId
illegalRoleInfo = InfoType { infoName = "illegal_role", infoDefault = 0, infoParse = fmap fromInteger . readEither }

memberRoleInfo :: InfoType RoleId
memberRoleInfo = InfoType { infoName = "member_role", infoDefault = 0, infoParse = fmap fromInteger . readEither }

visitorRoleInfo :: InfoType RoleId
visitorRoleInfo = InfoType { infoName = "visitor_role", infoDefault = 0, infoParse = fmap fromInteger . readEither }

addRemoveDiscordRoles :: MonadDiscord m => GuildId -> GuildMember -> [RoleId] -> [RoleId] -> m ()
addRemoveDiscordRoles gid GuildMember {..} universe correct = do
  let current = memberRoles `intersect` universe
  let toAdd = correct \\ current
  let toRemove = current \\ correct
  let did = maybe 0 userId memberUser
  for_ toAdd $ \r -> call_ $ R.AddGuildMemberRole gid did r
  for_ toRemove $ \r -> call_ $ R.RemoveGuildMemberRole gid did r

updateRolesDivisionTitle :: (MonadDiscord m, MonadCache InfoField m, MonadCache HypixelBowLeaderboardEntry m) => GuildMember -> Maybe BowBotAccount -> m ()
updateRolesDivisionTitle gmem (Just BowBotAccount {..}) = do
  lb <- getCacheMap (Proxy @HypixelBowLeaderboardEntry)
  let m = maximum . (0:) $ mapMaybe (fmap bowLbWins . (lb HM.!?)) accountMinecrafts
  divisionTitles <- hInfoDB divisionTitleRolesInfo
  let correctRole = fmap snd $ take 1 $ reverse $ filter (\(x,_) -> x <= m) divisionTitles
  gid <- hInfoDB discordGuildIdInfo
  addRemoveDiscordRoles gid gmem (map snd divisionTitles) correctRole
updateRolesDivisionTitle gmem Nothing = do
  pure () -- TODO

newtype SavedRoles = SavedRoles { getSavedRoleNames :: [String] }

instance Cached SavedRoles where
  type CacheIndex SavedRoles = UserId
  refreshCache conn _ = do
    cache <- getCache (Proxy @SavedRoles)
    res :: [(Integer, String)] <- queryLog conn "SELECT `discord`, `roles` FROM `unregisteredDEV` UNION SELECT `peopleDiscordDEV`.`discord`, `peopleDEV`.`roles` FROM `peopleDiscordDEV` JOIN `peopleDEV` ON `peopleDEV`.`id`=`peopleDiscordDEV`.`id`" ()
    let newValues = HM.fromList $ flip fmap res $ \(fromInteger -> did, SavedRoles . splitOn "," -> roles) -> (did, roles)
    liftIO $ atomically $ writeTVar cache newValues

updateRolesSaved :: (MonadDiscord m, MonadCache InfoField m, MonadCache SavedRoles m) => GuildMember -> m ()
updateRolesSaved gmem = do
  savedRolesAll <- hInfoDB savedRolesInfo
  savedRolesNames <- getFromCache (Proxy @SavedRoles) (maybe 0 userId (memberUser gmem))
  let savedRoles = mapMaybe (savedRolesAll M.!?) $ maybe [] getSavedRoleNames savedRolesNames
  gid <- hInfoDB discordGuildIdInfo
  addRemoveDiscordRoles gid gmem (map snd $ M.toList savedRolesAll) savedRoles

updateRolesMember :: (MonadNetwork m, MonadDiscord m, MonadCache InfoField m, MonadCacheSingle HypixelGuildMembers m, MonadCounter HypixelApi m) => GuildMember -> Maybe BowBotAccount -> m ()
updateRolesMember gmem (Just BowBotAccount {..}) = do
  m <- getHypixelGuildMembers
  let x = case m of
        CacheBusy -> Nothing
        CacheFailed -> Nothing
        CacheOld a -> Just a
        CacheFresh a -> Just a
  for_ x $ \(HypixelGuildMembers members) -> do
    let isMember = any (`elem` map fst (M.toList members)) accountMinecrafts
    memberRole <- hInfoDB memberRoleInfo
    visitorRole <- hInfoDB visitorRoleInfo
    let expectedRole = if isMember then memberRole else visitorRole
    gid <- hInfoDB discordGuildIdInfo
    addRemoveDiscordRoles gid gmem [memberRole, visitorRole] [expectedRole]
updateRolesMember gmem Nothing = do
  pure () -- TODO

updateRoles :: (MonadNetwork m, MonadDiscord m, MonadCache InfoField m, MonadCache HypixelBowLeaderboardEntry m, MonadCache SavedRoles m, MonadCacheSingle HypixelGuildMembers m, MonadCounter HypixelApi m) => GuildMember -> Maybe BowBotAccount -> m ()
updateRoles gmem acc = do
  updateRolesSaved gmem
  updateRolesDivisionTitle gmem acc
  updateRolesMember gmem acc

updateRolesAll :: (MonadNetwork m, MonadDiscord m, MonadCache InfoField m, MonadCache HypixelBowLeaderboardEntry m, MonadCache BowBotAccount m, MonadCache SavedRoles m, MonadCacheSingle HypixelGuildMembers m, MonadCounter HypixelApi m) => m ()
updateRolesAll = do
  gid <- hInfoDB discordGuildIdInfo
  mems <- discordGuildMembers gid
  for_ mems $ \gmem -> do
    accs <- getBowBotAccountByDiscord (maybe 0 userId (memberUser gmem))
    updateRoles gmem accs


