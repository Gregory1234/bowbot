{-# LANGUAGE QuasiQuotes #-}

module BowBot.Discord.Roles where

import qualified Data.Map as M
import BowBot.Hypixel.Leaderboard
import Discord.Types
import BowBot.BotData.Info
import BowBot.Discord.Utils
import BowBot.Counter.Basic
import BowBot.Account.Basic
import qualified Data.HashMap.Strict as HM
import BowBot.DB.Basic
import BowBot.Network.Basic
import qualified Data.Text as T
import Data.Bifunctor (first)
import BowBot.Minecraft.Basic (UUID)
import BowBot.Discord.SavedRoles
import BowBot.Hypixel.Basic
import Data.Ord (Down(..))
import BowBot.Perms.Basic
import BowBot.Discord.PermsRoles


divisionTitleRolesInfo :: InfoType [(Integer, RoleId)]
divisionTitleRolesInfo = InfoType { infoName = "division_title_roles", infoDefault = [], infoParse = \s -> for (T.lines s) $ \l -> case T.splitOn "->" l of [a, b] -> (,) <$> (first pack . readEither . unpack) a <*> fmap fromInteger ((first pack . readEither . unpack) b); _ -> Left "wrong format" }

bowWinsLbSpotRolesInfo :: InfoType [(Integer, RoleId)]
bowWinsLbSpotRolesInfo = InfoType { infoName = "bow_wins_lb_spot_roles", infoDefault = [], infoParse = \s -> for (T.lines s) $ \l -> case T.splitOn "->" l of [a, b] -> (,) <$> (first pack . readEither . unpack) a <*> fmap fromInteger ((first pack . readEither . unpack) b); _ -> Left "wrong format" }

bowWinstreakLbSpotRolesInfo :: InfoType [(Integer, RoleId)]
bowWinstreakLbSpotRolesInfo = InfoType { infoName = "bow_winstreak_lb_spot_roles", infoDefault = [], infoParse = \s -> for (T.lines s) $ \l -> case T.splitOn "->" l of [a, b] -> (,) <$> (first pack . readEither . unpack) a <*> fmap fromInteger ((first pack . readEither . unpack) b); _ -> Left "wrong format" }

illegalRoleInfo :: InfoType RoleId
illegalRoleInfo = InfoType { infoName = "illegal_role", infoDefault = 0, infoParse = fmap fromInteger . (first pack . readEither . unpack) }

memberRoleInfo :: InfoType RoleId
memberRoleInfo = InfoType { infoName = "member_role", infoDefault = 0, infoParse = fmap fromInteger . (first pack . readEither . unpack) }

visitorRoleInfo :: InfoType RoleId
visitorRoleInfo = InfoType { infoName = "visitor_role", infoDefault = 0, infoParse = fmap fromInteger . (first pack . readEither . unpack) }

giveRolesDivisionTitle :: (MonadIOReader m r, HasAll '[SafeMysqlConn, DiscordHandle, InfoCache] r) => GuildMember -> Integer -> m ()
giveRolesDivisionTitle gmem maxWins = do
  gid <- askInfo discordGuildIdInfo
  allRoles <- askInfo divisionTitleRolesInfo
  let selectedRole = map snd $ take 1 $ reverse $ takeWhile ((<= maxWins) . fst) allRoles
  addRemoveDiscordRoles gid gmem (map snd allRoles) selectedRole

applyRolesDivisionTitleByBowBotId' :: (MonadIOReader m r, HasAll '[SafeMysqlConn, DiscordHandle, InfoCache] r) => BowBotId -> [GuildMember] -> m ()
applyRolesDivisionTitleByBowBotId' bid gmems = do
  wins :: [Integer] <- queryLog [mysql|SELECT `wins` FROM `hypixel_bow_stats` JOIN `account_minecraft` ON `minecraft_uuid` = `hypixel_bow_stats`.`minecraft_uuid` WHERE `account_id` = bid|]
  for_ gmems $ \gmem -> do
    giveRolesDivisionTitle gmem (foldl' max 0 wins)

applyRolesDivisionTitleByBowBotId :: (MonadIOReader m r, HasAll '[SafeMysqlConn, DiscordHandle, InfoCache] r) => BowBotId -> m ()
applyRolesDivisionTitleByBowBotId bid = do
  gid <- askInfo discordGuildIdInfo
  gmems <- discordGuildMembers gid
  accountDiscords <- getDiscordIdsByBowBotId bid
  applyRolesDivisionTitleByBowBotId' bid $ filter (\gmem -> maybe 0 userId (memberUser gmem) `elem` accountDiscords) gmems

applyRolesDivisionTitleByUUID :: (MonadIOReader m r, HasAll '[SafeMysqlConn, DiscordHandle, InfoCache] r) => UUID -> m ()
applyRolesDivisionTitleByUUID uuid = do
  bbacc <- getBowBotIdByMinecraft uuid
  for_ bbacc applyRolesDivisionTitleByBowBotId

giveRolesMember :: (MonadIOReader m r, HasAll '[DiscordHandle, InfoCache] r) => GuildMember -> Bool -> m ()
giveRolesMember gmem isMember = do
  memberRole <- askInfo memberRoleInfo
  visitorRole <- askInfo visitorRoleInfo
  let expectedRole = if isMember then memberRole else visitorRole
  gid <- askInfo discordGuildIdInfo
  addRemoveDiscordRoles gid gmem [memberRole, visitorRole] [expectedRole]

giveRolesMemberUnregistered :: (MonadIOReader m r, HasAll '[DiscordHandle, InfoCache] r) => GuildMember -> m ()
giveRolesMemberUnregistered gmem = do
  memberRole <- askInfo memberRoleInfo
  visitorRole <- askInfo visitorRoleInfo
  gid <- askInfo discordGuildIdInfo
  unless (memberRole `elem` memberRoles gmem) $ addRemoveDiscordRoles gid gmem [visitorRole] [visitorRole]

removeIllegalRole :: (MonadIOReader m r, HasAll '[DiscordHandle, InfoCache] r) => GuildMember -> m ()
removeIllegalRole gmem = do
  illegalRole <- askInfo illegalRoleInfo
  gid <- askInfo discordGuildIdInfo
  addRemoveDiscordRoles gid gmem [illegalRole] []

giveIllegalRole :: (MonadIOReader m r, HasAll '[DiscordHandle, InfoCache] r) => GuildMember -> m ()
giveIllegalRole gmem = do
  memberRole <- askInfo memberRoleInfo
  savedHypixelRoles <- map (snd . snd) . M.toList <$> askInfo savedHypixelRolesInfo
  divisionTitleRoles <- map snd <$> askInfo divisionTitleRolesInfo
  winsLbRoles <- map snd <$> askInfo bowWinsLbSpotRolesInfo
  winstreakLbRoles <- map snd <$> askInfo bowWinstreakLbSpotRolesInfo
  illegalRole <- askInfo illegalRoleInfo
  gid <- askInfo discordGuildIdInfo
  addRemoveDiscordRoles gid gmem [illegalRole] $ [illegalRole | not . null $ intersect (memberRoles gmem) (memberRole:savedHypixelRoles ++ divisionTitleRoles ++ winsLbRoles ++ winstreakLbRoles)]

-- TODO: clean this up

applyRolesByBowBotId' :: (MonadIOReader m r, HasAll '[SafeMysqlConn, DiscordHandle, Manager, CounterState, InfoCache] r) => Maybe BowBotId -> [GuildMember] -> m ()
applyRolesByBowBotId' (Just bid) gmems = do
  wins :: [Integer] <- queryLog [mysql|SELECT `wins` FROM `hypixel_bow_stats` JOIN `account_minecraft` ON `minecraft_uuid` = `hypixel_bow_stats`.`minecraft_uuid` WHERE `account_id` = bid|]
  savedRoles :: [SavedRole] <- fromMaybe [] <$> queryOnlyLog [mysql|SELECT `roles` FROM `account` WHERE `id` = bid|]
  -- TODO: support smart casts for IS NOT NULL
  hypixelRoles :: [HypixelRole] <- queryLog [mysql|SELECT `hypixel_role` OVERRIDE HypixelRole FROM `minecraft` JOIN `account_minecraft` ON `minecraft_uuid` = `minecraft`.`uuid` WHERE `account_minecraft`.`account_id` = bid AND `hypixel_role` <> NULL|]
  for_ gmems $ \gmem -> do
    perms <- traverse (getPermissionLevelByDiscord . userId) $ memberUser gmem
    givePermsRoles gmem $ fromMaybe DefaultLevel perms
    giveSavedRoles gmem savedRoles (Just hypixelRoles)
    giveRolesDivisionTitle gmem (foldl' max 0 wins)
    giveRolesMember gmem (not $ null hypixelRoles)
    removeIllegalRole gmem
applyRolesByBowBotId' Nothing gmems = for_ gmems $ \gmem -> do
  giveRolesMemberUnregistered gmem
  giveIllegalRole gmem


applyRolesByBowBotId :: (MonadIOReader m r, HasAll '[SafeMysqlConn, DiscordHandle, Manager, CounterState, InfoCache] r) => BowBotId -> m ()
applyRolesByBowBotId bid = do
  gid <- askInfo discordGuildIdInfo
  gmems <- discordGuildMembers gid
  accountDiscords <- getDiscordIdsByBowBotId bid
  applyRolesByBowBotId' (Just bid) $ filter (\gmem -> maybe 0 userId (memberUser gmem) `elem` accountDiscords) gmems

applyRoles :: (MonadIOReader m r, HasAll '[SafeMysqlConn, DiscordHandle, Manager, CounterState, InfoCache] r) => GuildMember -> m ()
applyRoles gmem = do
  bid <- getBowBotIdByDiscord (maybe 0 userId (memberUser gmem))
  applyRolesByBowBotId' bid [gmem]

applyRolesAll :: (MonadIOReader m r, HasAll '[SafeMysqlConn, DiscordHandle, Manager, CounterState, InfoCache] r) => m ()
applyRolesAll = do
  lb <- getHypixelBowLeaderboards
  -- TODO: support smart casts for IS NOT NULL
  savedRoles :: M.Map UserId [SavedRole] <- M.fromList <$> queryLog [mysql|SELECT `discord_id`, `roles` FROM `account` JOIN `account_discord` ON `account_id` = `account`.`id`|]
  permissions :: M.Map UserId PermissionLevel <- M.fromList <$> queryLog [mysql|SELECT `discord_id`, `level` FROM `permissions`|]
  gid <- askInfo discordGuildIdInfo
  gmems <- discordGuildMembers gid
  roles :: [(UUID, HypixelRole)] <- queryLog [mysql|SELECT `uuid`, `hypixel_role` OVERRIDE HypixelRole FROM `minecraft` WHERE `hypixel_role` <> NULL|]
  accountMinecrafts :: M.Map UserId [UUID] <- M.map (map snd) . groupByToMap fst <$> queryLog [mysql|SELECT `account_discord`.`discord_id`, `account_minecraft`.`minecraft_uuid` FROM `account_minecraft` JOIN `account_discord` ON `account_id` = `account_minecraft`.`account_id`|]
  for_ gmems $ \gmem -> do
    let discord = maybe 0 userId (memberUser gmem)
    case accountMinecrafts M.!? discord of
      Nothing -> do
        givePermsRoles gmem $ fromMaybe DefaultLevel (permissions M.!? discord)
        for_ (savedRoles M.!? discord) $ \r -> giveSavedRoles gmem r Nothing
        giveRolesMemberUnregistered gmem -- TODO: they might not be unregistered, this is a bad name
        giveIllegalRole gmem
      Just mcs -> do
        givePermsRoles gmem $ fromMaybe DefaultLevel (permissions M.!? discord)
        let hypixelRoles = mapMaybe (`lookup` roles) mcs
        for_ (savedRoles M.!? discord) $ \r -> giveSavedRoles gmem r (Just hypixelRoles)
        giveRolesDivisionTitle gmem (foldl' max 0 (mapMaybe (fmap bowLbWins . (lb HM.!?)) mcs))
        giveRolesMember gmem (not $ null hypixelRoles)
        removeIllegalRole gmem
  applyLeaderboardRoles gmems accountMinecrafts

applyLeaderboardRolesSingle :: (MonadIOReader m r, HasAll '[DiscordHandle, InfoCache] r) => [GuildMember] -> M.Map UserId [UUID] -> M.Map UUID Integer -> [(Integer, RoleId)] -> m ()
applyLeaderboardRolesSingle gmems accountMinecrafts places roles = do
  gid <- askInfo discordGuildIdInfo
  for_ gmems $ \gmem -> do
    let discord = maybe 0 userId (memberUser gmem)
    case accountMinecrafts M.!? discord of
      Nothing -> pure ()
      Just mcs -> do
        let bestPlace = foldl' min 100000000 $ mapMaybe (places M.!?) mcs
        let placeRoles = map snd $ dropWhile ((< bestPlace) . fst) roles
        addRemoveDiscordRoles gid gmem (map snd roles) placeRoles

applyLeaderboardRoles :: (MonadIOReader m r, HasAll '[SafeMysqlConn, DiscordHandle, InfoCache] r) => [GuildMember] -> M.Map UserId [UUID] -> m ()
applyLeaderboardRoles gmems mcs = do
  bowLb <- getHypixelBowLeaderboards
  winsRoles <- askInfo bowWinsLbSpotRolesInfo
  let winsLb = M.fromList . zipWith (\n (uuid, _) -> (uuid, n)) [1..] . sortOn (Down . bowLbWins . snd) $ HM.toList bowLb -- TODO: what if multiple people have the same score?
  applyLeaderboardRolesSingle gmems mcs winsLb winsRoles
  winstreakRoles <- askInfo bowWinstreakLbSpotRolesInfo
  let winstreakLb = M.fromList . zipWith (\n (uuid, _) -> (uuid, n)) [1..] . sortOn (Down . bowLbWinstreak . snd) $ HM.toList bowLb
  applyLeaderboardRolesSingle gmems mcs winstreakLb winstreakRoles