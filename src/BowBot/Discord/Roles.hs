{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module BowBot.Discord.Roles where

import qualified Data.Map as M
import BowBot.Hypixel.Leaderboard
import Discord.Types
import BowBot.BotData.Info
import BowBot.Discord.Utils
import BowBot.BotData.Cached
import BowBot.BotData.CachedSingle
import BowBot.Counter.Basic
import BowBot.Account.Basic
import qualified Data.HashMap.Strict as HM
import qualified Discord.Requests as R
import BowBot.DB.Basic (queryLog, withDB, executeManyLog', Connection)
import BowBot.Hypixel.Guild
import BowBot.Network.Basic
import qualified Data.Text as T
import Data.Bifunctor (first)
import BowBot.Minecraft.Basic (UUID)



divisionTitleRolesInfo :: InfoType [(Integer, RoleId)]
divisionTitleRolesInfo = InfoType { infoName = "division_title_roles", infoDefault = [], infoParse = \s -> for (T.lines s) $ \l -> case T.splitOn "->" l of [a, b] -> (,) <$> (first pack . readEither . unpack) a <*> fmap fromInteger ((first pack . readEither . unpack) b); _ -> Left "wrong format" }

toggleableRolesInfo :: InfoType (M.Map Text RoleId)
toggleableRolesInfo = InfoType { infoName = "toggleable_roles", infoDefault = M.empty, infoParse = \s -> fmap M.fromList $ for (T.lines s) $ \l -> case T.splitOn "->" l of [a, b] -> (a,) <$> fmap fromInteger ((first pack . readEither . unpack) b); _ -> Left "wrong format" }

savedRolesInfo :: InfoType (M.Map Text RoleId)
savedRolesInfo = InfoType { infoName = "saved_roles", infoDefault = M.empty, infoParse = \s -> fmap M.fromList $ for (T.lines s) $ \l -> case T.splitOn "->" l of [a, b] -> (a,) <$> fmap fromInteger ((first pack . readEither . unpack) b); _ -> Left "wrong format" }

savedHypixelRolesInfo :: InfoType (M.Map Text ([Text], RoleId))
savedHypixelRolesInfo = InfoType { infoName = "hypixel_roles", infoDefault = M.empty, infoParse = \s -> fmap M.fromList $ for (T.lines s) $ \l -> case T.splitOn "->" l of [a, b, c] -> (a,) . (T.splitOn "|" b,) <$> fmap fromInteger ((first pack . readEither . unpack) c); _ -> Left "wrong format" }

illegalRoleInfo :: InfoType RoleId
illegalRoleInfo = InfoType { infoName = "illegal_role", infoDefault = 0, infoParse = fmap fromInteger . (first pack . readEither . unpack) }

memberRoleInfo :: InfoType RoleId
memberRoleInfo = InfoType { infoName = "member_role", infoDefault = 0, infoParse = fmap fromInteger . (first pack . readEither . unpack) }

visitorRoleInfo :: InfoType RoleId
visitorRoleInfo = InfoType { infoName = "visitor_role", infoDefault = 0, infoParse = fmap fromInteger . (first pack . readEither . unpack) }

addRemoveDiscordRoles :: (MonadIOReader m r, Has DiscordHandle r) => GuildId -> GuildMember -> [RoleId] -> [RoleId] -> m ()
addRemoveDiscordRoles gid GuildMember {..} universe correct = do
  let current = memberRoles `intersect` universe
  let toAdd = correct \\ current
  let toRemove = current \\ correct
  let did = maybe 0 userId memberUser
  for_ toAdd $ \r -> call_ $ R.AddGuildMemberRole gid did r
  for_ toRemove $ \r -> call_ $ R.RemoveGuildMemberRole gid did r

updateRolesDivisionTitle :: (MonadIOBotData m d r, HasAll [DiscordHandle, Connection] r, HasCache InfoField d) => GuildMember -> Maybe BowBotAccount -> m ()
updateRolesDivisionTitle gmem (Just BowBotAccount {..}) = do
  lb <- getHypixelBowLeaderboards
  let m = maximum . (0:) $ mapMaybe (fmap bowLbWins . (lb HM.!?)) accountMinecrafts
  divisionTitles <- askInfo divisionTitleRolesInfo
  let correctRole = fmap snd $ take 1 $ reverse $ filter (\(x,_) -> x <= m) divisionTitles
  gid <- askInfo discordGuildIdInfo
  addRemoveDiscordRoles gid gmem (map snd divisionTitles) correctRole
updateRolesDivisionTitle _ Nothing = pure ()

updateRolesDivisionTitleByUUID :: (MonadIOBotData m d r, HasAll [DiscordHandle, Connection] r, HasCaches [InfoField, BowBotAccount] d) => UUID -> m ()
updateRolesDivisionTitleByUUID mcUUID = do
  gid <- askInfo discordGuildIdInfo
  gmems <- discordGuildMembers gid
  acc' <- getBowBotAccountByMinecraft mcUUID
  for_ acc' $ \acc -> for_ gmems $ \gmem -> when (maybe 0 userId (memberUser gmem) `elem` accountDiscords acc) $ updateRolesDivisionTitle gmem (Just acc)

newtype SavedRoles = SavedRoles { getSavedRoleNames :: [Text] } deriving (Show, Eq)

instance Cached SavedRoles where
  type CacheIndex SavedRoles = UserId
  refreshCache = do
    cache <- getCache
    res :: [(UserId, Text)] <- queryLog "SELECT `discord`, `roles` FROM `unregistered` UNION SELECT `peopleDiscord`.`discord`, `people`.`roles` FROM `peopleDiscord` JOIN `people` ON `people`.`id`=`peopleDiscord`.`id`" ()
    let newValues = HM.fromList $ flip fmap res $ \(did, SavedRoles . T.splitOn "," -> roles) -> (did, roles)
    liftIO $ atomically $ writeTVar cache newValues

storeNewRolesSaved :: (MonadIOBotData m d r, HasCaches [InfoField, SavedRoles, BowBotAccount] d) => UserId -> [RoleId] -> m ()
storeNewRolesSaved did roles = do
  old <- getFromCache did
  toggleableRolesAll <- askInfo toggleableRolesInfo
  savedRolesAll <- askInfo savedRolesInfo
  savedHypixelRolesAll <- askInfo savedHypixelRolesInfo
  let rolesAll = M.fromList . map (\(x,y) -> (y,x)) . M.toList $ toggleableRolesAll <> savedRolesAll <> M.map snd savedHypixelRolesAll
  let savedRoles = SavedRoles $ mapMaybe (rolesAll M.!?) roles
  let rolesStr = T.intercalate "," $ getSavedRoleNames savedRoles
  when (old /= Just savedRoles) $ do
    acc <- getBowBotAccountByDiscord did
    case acc of
      Nothing -> do
        success <- liftIO $ withDB $ \conn -> (>0) <$> executeManyLog' conn "INSERT INTO `unregistered` (`discord`, `roles`) VALUES (?,?) ON DUPLICATE KEY UPDATE `roles`=VALUES(`roles`)" [(toInteger did, rolesStr)]
        when success $ do
          cache <- getCache
          liftIO $ atomically $ modifyTVar cache (insertMany [(did, savedRoles)])
      Just a -> do
        success <- liftIO $ withDB $ \conn -> (>0) <$> executeManyLog' conn "INSERT INTO `people` (`id`, `roles`) VALUES (?,?) ON DUPLICATE KEY UPDATE `roles`=VALUES(`roles`)" [(accountBotId a, rolesStr)]
        when success $ do
          cache <- getCache
          liftIO $ atomically $ modifyTVar cache (insertMany $ map (,savedRoles) (accountDiscords a))

storeNewSavedRolesAll :: (MonadIOBotData m d r, Has DiscordHandle r, HasCaches [InfoField, SavedRoles, BowBotAccount] d) => m ()
storeNewSavedRolesAll = do
  gid <- askInfo discordGuildIdInfo
  mems <- discordGuildMembers gid
  for_ mems $ \gmem -> do
    storeNewRolesSaved (maybe 0 userId (memberUser gmem)) (memberRoles gmem)

updateRolesSaved :: (MonadHoistIOBotData m d r, HasAll [DiscordHandle, Manager, CounterState] r, HasCaches [InfoField, SavedRoles] d, HasCachedData HypixelGuildMembers d) => GuildMember -> Maybe BowBotAccount -> m ()
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

updateRolesMember :: (MonadHoistIOBotData m d r, HasAll [DiscordHandle, Manager, CounterState] r, HasCache InfoField d, HasCachedData HypixelGuildMembers d) => GuildMember -> Maybe BowBotAccount -> m ()
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

updateRolesIllegal :: (MonadIOBotData m d r, Has DiscordHandle r, HasCache InfoField d) => GuildMember -> Maybe BowBotAccount -> m ()
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

updateRoles :: (MonadHoistIOBotData m d r, HasAll [DiscordHandle, CounterState, Connection] r, Has Manager r, HasCache InfoField d, HasCache SavedRoles d, HasCachedData HypixelGuildMembers d) => GuildMember -> Maybe BowBotAccount -> m ()
updateRoles gmem acc = do
  updateRolesSaved gmem acc
  updateRolesDivisionTitle gmem acc
  updateRolesMember gmem acc
  updateRolesIllegal gmem acc

updateRolesAll :: (MonadHoistIOBotData m d r, HasAll [DiscordHandle, Manager, CounterState, Connection] r, HasCaches [InfoField, BowBotAccount, SavedRoles] d, HasCachedData HypixelGuildMembers d) => m ()
updateRolesAll = do
  gid <- askInfo discordGuildIdInfo
  mems <- discordGuildMembers gid
  for_ mems $ \gmem -> do
    accs <- getBowBotAccountByDiscord (maybe 0 userId (memberUser gmem))
    updateRoles gmem accs


