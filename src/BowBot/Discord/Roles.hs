{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
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
import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe)
import qualified Discord.Requests as R
import Data.List ((\\), intersect, intercalate)
import BowBot.DB.Basic (queryLog, withDB, executeManyLog)
import BowBot.Hypixel.Guild
import BowBot.Network.Basic
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

addRemoveDiscordRoles :: (MonadIO m, MonadReader r m, Has DiscordHandle r) => GuildId -> GuildMember -> [RoleId] -> [RoleId] -> m ()
addRemoveDiscordRoles gid GuildMember {..} universe correct = do
  let current = memberRoles `intersect` universe
  let toAdd = correct \\ current
  let toRemove = current \\ correct
  let did = maybe 0 userId memberUser
  for_ toAdd $ \r -> call_ $ R.AddGuildMemberRole gid did r
  for_ toRemove $ \r -> call_ $ R.RemoveGuildMemberRole gid did r

updateRolesDivisionTitle :: (MonadIO m, MonadReader r m, Has DiscordHandle r, HasCache InfoField r, HasCache HypixelBowLeaderboardEntry r) => GuildMember -> Maybe BowBotAccount -> m ()
updateRolesDivisionTitle gmem (Just BowBotAccount {..}) = do
  lb <- getCacheMap
  let m = maximum . (0:) $ mapMaybe (fmap bowLbWins . (lb HM.!?)) accountMinecrafts
  divisionTitles <- askInfo divisionTitleRolesInfo
  let correctRole = fmap snd $ take 1 $ reverse $ filter (\(x,_) -> x <= m) divisionTitles
  gid <- askInfo discordGuildIdInfo
  addRemoveDiscordRoles gid gmem (map snd divisionTitles) correctRole
updateRolesDivisionTitle _ Nothing = pure ()

newtype SavedRoles = SavedRoles { getSavedRoleNames :: [String] } deriving (Show, Eq)

instance Cached SavedRoles where
  type CacheIndex SavedRoles = UserId
  refreshCache conn = do
    cache <- getCache
    res :: [(Integer, String)] <- queryLog conn "SELECT `discord`, `roles` FROM `unregisteredDEV` UNION SELECT `peopleDiscordDEV`.`discord`, `peopleDEV`.`roles` FROM `peopleDiscordDEV` JOIN `peopleDEV` ON `peopleDEV`.`id`=`peopleDiscordDEV`.`id`" ()
    let newValues = HM.fromList $ flip fmap res $ \(fromInteger -> did, SavedRoles . splitOn "," -> roles) -> (did, roles)
    liftIO $ atomically $ writeTVar cache newValues

storeNewRolesSaved :: (MonadIO m, MonadReader r m, HasCache InfoField r, HasCache SavedRoles r, HasCache BowBotAccount r) => UserId -> [RoleId] -> m ()
storeNewRolesSaved did roles = do
  old <- getFromCache did
  toggleableRolesAll <- askInfo toggleableRolesInfo
  savedRolesAll <- askInfo savedRolesInfo
  savedHypixelRolesAll <- askInfo savedHypixelRolesInfo
  let rolesAll = M.fromList . map (\(x,y) -> (y,x)) . M.toList $ toggleableRolesAll <> savedRolesAll <> M.map snd savedHypixelRolesAll
  let savedRoles = SavedRoles $ mapMaybe (rolesAll M.!?) roles
  let rolesStr = intercalate "," $ getSavedRoleNames savedRoles
  when (old /= Just savedRoles) $ do
    acc <- getBowBotAccountByDiscord did
    case acc of
      Nothing -> do
        success <- liftIO $ withDB $ \conn -> (>0) <$> executeManyLog conn "INSERT INTO `unregisteredDEV` (`discord`, `roles`) VALUES (?,?) ON DUPLICATE KEY UPDATE `roles`=VALUES(`roles`)" [(toInteger did, rolesStr)]
        when success $ do
          cache <- getCache
          liftIO $ atomically $ modifyTVar cache (insertMany [(did, savedRoles)])
      Just a -> do
        success <- liftIO $ withDB $ \conn -> (>0) <$> executeManyLog conn "INSERT INTO `peopleDEV` (`id`, `roles`) VALUES (?,?) ON DUPLICATE KEY UPDATE `roles`=VALUES(`roles`)" [(BowBot.Account.Basic.accountId a, rolesStr)]
        when success $ do
          cache <- getCache
          liftIO $ atomically $ modifyTVar cache (insertMany $ map (,savedRoles) (accountDiscords a))

storeNewSavedRolesAll :: (MonadIO m, MonadReader r m, Has DiscordHandle r, HasCache InfoField r, HasCache SavedRoles r, HasCache BowBotAccount r) => m ()
storeNewSavedRolesAll = do
  gid <- askInfo discordGuildIdInfo
  mems <- discordGuildMembers gid
  for_ mems $ \gmem -> do
    storeNewRolesSaved (maybe 0 userId (memberUser gmem)) (memberRoles gmem)

updateRolesSaved :: (MonadHoistIO m, MonadReader r m, Has DiscordHandle r, Has Manager r, HasCache InfoField r, HasCache SavedRoles r, HasCachedData HypixelGuildMembers r, HasCounter HypixelApi r) => GuildMember -> Maybe BowBotAccount -> m ()
updateRolesSaved gmem acc = do
  toggleableRolesAll <- askInfo toggleableRolesInfo
  savedRolesAll <- askInfo savedRolesInfo
  savedHypixelRolesAll <- askInfo savedHypixelRolesInfo
  let rolesAll = toggleableRolesAll <> savedRolesAll <> M.map snd savedHypixelRolesAll
  savedRolesNames <- getFromCache (maybe 0 userId (memberUser gmem))
  let savedRoles = mapMaybe (rolesAll M.!?) $ maybe [] getSavedRoleNames savedRolesNames
  gid <- askInfo discordGuildIdInfo
  case acc of
    Nothing -> addRemoveDiscordRoles gid gmem (map snd $ M.toList rolesAll) savedRoles
    Just a -> do
      hypixelGuild <- if null (savedRoles `intersect` map snd (M.elems savedHypixelRolesAll)) then pure (CacheFresh (HypixelGuildMembers M.empty)) else getHypixelGuildMembers
      let g = case hypixelGuild of
            CacheBusy -> Nothing
            CacheFailed -> Nothing
            CacheFresh m -> Just m
            CacheOld m -> Just m
      case g of
        Just (HypixelGuildMembers mems) -> do
          let hypixelRoles = mapMaybe (mems M.!?) (accountMinecrafts a)
          let allowed = M.elems toggleableRolesAll ++ M.elems savedRolesAll ++ map snd (filter (any (`elem` hypixelRoles) . fst) $ M.elems savedHypixelRolesAll)
          addRemoveDiscordRoles gid gmem (map snd $ M.toList rolesAll) (savedRoles `intersect` allowed)
        Nothing -> addRemoveDiscordRoles gid gmem (map snd $ M.toList rolesAll) savedRoles

updateRolesMember :: (MonadHoistIO m, MonadReader r m, Has DiscordHandle r, Has Manager r, HasCache InfoField r, HasCachedData HypixelGuildMembers r, HasCounter HypixelApi r) => GuildMember -> Maybe BowBotAccount -> m ()
updateRolesMember gmem (Just BowBotAccount {..}) = do
  m <- getHypixelGuildMembers
  let x = case m of
        CacheBusy -> Nothing
        CacheFailed -> Nothing
        CacheOld a -> Just a
        CacheFresh a -> Just a
  for_ x $ \(HypixelGuildMembers members) -> do
    let isMember = any (`elem` map fst (M.toList members)) accountMinecrafts
    memberRole <- askInfo memberRoleInfo
    visitorRole <- askInfo visitorRoleInfo
    let expectedRole = if isMember then memberRole else visitorRole
    gid <- askInfo discordGuildIdInfo
    addRemoveDiscordRoles gid gmem [memberRole, visitorRole] [expectedRole]
updateRolesMember gmem Nothing = do
  memberRole <- askInfo memberRoleInfo
  visitorRole <- askInfo visitorRoleInfo
  gid <- askInfo discordGuildIdInfo
  unless (memberRole `elem` memberRoles gmem) $ addRemoveDiscordRoles gid gmem [visitorRole] [visitorRole]

updateRolesIllegal :: (MonadIO m, MonadReader r m, Has DiscordHandle r, HasCache InfoField r) => GuildMember -> Maybe BowBotAccount -> m ()
updateRolesIllegal gmem (Just _) = do
  illegalRole <- askInfo illegalRoleInfo
  gid <- askInfo discordGuildIdInfo
  addRemoveDiscordRoles gid gmem [illegalRole] []
updateRolesIllegal gmem Nothing = do
  illegalRole <- askInfo illegalRoleInfo
  savedHypixelRolesAll <- map snd . M.elems <$> askInfo savedHypixelRolesInfo
  divisionRoles <- map snd <$> askInfo divisionTitleRolesInfo
  memberRole <- askInfo memberRoleInfo
  let allRoles = savedHypixelRolesAll ++ divisionRoles ++ [memberRole]
  gid <- askInfo discordGuildIdInfo
  addRemoveDiscordRoles gid gmem [illegalRole] [illegalRole | not (null (memberRoles gmem `intersect` allRoles))]

updateRoles :: (MonadHoistIO m, MonadReader r m, Has DiscordHandle r, Has Manager r, HasCache InfoField r, HasCache HypixelBowLeaderboardEntry r, HasCache SavedRoles r, HasCachedData HypixelGuildMembers r, HasCounter HypixelApi r) => GuildMember -> Maybe BowBotAccount -> m ()
updateRoles gmem acc = do
  updateRolesSaved gmem acc
  updateRolesDivisionTitle gmem acc
  updateRolesMember gmem acc
  updateRolesIllegal gmem acc

updateRolesAll :: (MonadHoistIO m, MonadReader r m, Has DiscordHandle r, Has Manager r, HasCache InfoField r, HasCache HypixelBowLeaderboardEntry r, HasCache BowBotAccount r, HasCache SavedRoles r, HasCachedData HypixelGuildMembers r, HasCounter HypixelApi r) => m ()
updateRolesAll = do
  gid <- askInfo discordGuildIdInfo
  mems <- discordGuildMembers gid
  for_ mems $ \gmem -> do
    accs <- getBowBotAccountByDiscord (maybe 0 userId (memberUser gmem))
    updateRoles gmem accs


