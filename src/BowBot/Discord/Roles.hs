module BowBot.Discord.Roles where

import qualified Data.Map as M
import BowBot.Hypixel.Leaderboard
import Discord.Types
import BowBot.BotData.Info
import BowBot.Discord.Utils
import BowBot.BotData.Cached
import BowBot.Counter.Basic
import BowBot.Account.Basic
import qualified Data.HashMap.Strict as HM
import BowBot.DB.Basic (queryLog, Connection, Only(..))
import BowBot.Hypixel.Guild
import BowBot.Network.Basic
import qualified Data.Text as T
import Data.Bifunctor (first)
import BowBot.Minecraft.Basic (UUID)
import BowBot.Discord.SavedRoles
import Database.MySQL.Simple (In(..))



divisionTitleRolesInfo :: InfoType [(Integer, RoleId)]
divisionTitleRolesInfo = InfoType { infoName = "division_title_roles", infoDefault = [], infoParse = \s -> for (T.lines s) $ \l -> case T.splitOn "->" l of [a, b] -> (,) <$> (first pack . readEither . unpack) a <*> fmap fromInteger ((first pack . readEither . unpack) b); _ -> Left "wrong format" }

illegalRoleInfo :: InfoType RoleId
illegalRoleInfo = InfoType { infoName = "illegal_role", infoDefault = 0, infoParse = fmap fromInteger . (first pack . readEither . unpack) }

memberRoleInfo :: InfoType RoleId
memberRoleInfo = InfoType { infoName = "member_role", infoDefault = 0, infoParse = fmap fromInteger . (first pack . readEither . unpack) }

visitorRoleInfo :: InfoType RoleId
visitorRoleInfo = InfoType { infoName = "visitor_role", infoDefault = 0, infoParse = fmap fromInteger . (first pack . readEither . unpack) }

giveRolesDivisionTitle :: (MonadIOReader m r, HasAll '[Connection, DiscordHandle, InfoCache] r) => GuildMember -> Integer -> m ()
giveRolesDivisionTitle gmem maxWins = do
  gid <- askInfo discordGuildIdInfo
  allRoles <- askInfo divisionTitleRolesInfo
  let selectedRole = map snd $ take 1 $ reverse $ takeWhile ((<= maxWins) . fst) allRoles
  addRemoveDiscordRoles gid gmem (map snd allRoles) selectedRole

applyRolesDivisionTitleByBowBotId' :: (MonadIOReader m r, HasAll '[Connection, DiscordHandle, InfoCache] r) => BowBotId -> [GuildMember] -> m ()
applyRolesDivisionTitleByBowBotId' bid gmems = do
  wins :: [Integer] <- map fromOnly <$> queryLog "SELECT `bowWins` FROM `stats` JOIN `peopleMinecraft` ON `stats`.`minecraft` = `peopleMinecraft`.`minecraft` WHERE `id` = ?" (Only bid)
  for_ gmems $ \gmem -> do
    giveRolesDivisionTitle gmem (foldl' max 0 wins)

applyRolesDivisionTitleByBowBotId :: (MonadIOReader m r, HasAll '[Connection, DiscordHandle, InfoCache] r) => BowBotId -> m ()
applyRolesDivisionTitleByBowBotId bid = do
  gid <- askInfo discordGuildIdInfo
  gmems <- discordGuildMembers gid
  accountDiscords <- getDiscordIdsByBowBotId bid
  applyRolesDivisionTitleByBowBotId' bid $ filter (\gmem -> maybe 0 userId (memberUser gmem) `elem` accountDiscords) gmems

applyRolesDivisionTitleByUUID :: (MonadIOReader m r, HasAll '[Connection, DiscordHandle, InfoCache] r) => UUID -> m ()
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
  illegalRole <- askInfo illegalRoleInfo
  gid <- askInfo discordGuildIdInfo
  addRemoveDiscordRoles gid gmem [illegalRole] $ [illegalRole | not . null $ intersect (memberRoles gmem) (memberRole:savedHypixelRoles ++ divisionTitleRoles)]

applyRolesByBowBotId' :: (MonadIOReader m r, HasAll '[Connection, DiscordHandle, Manager, CounterState, InfoCache] r) => Maybe BowBotId -> [GuildMember] -> m ()
applyRolesByBowBotId' (Just bid) gmems = do
  wins :: [Integer] <- map fromOnly <$> queryLog "SELECT `bowWins` FROM `stats` JOIN `peopleMinecraft` ON `stats`.`minecraft` = `peopleMinecraft`.`minecraft` WHERE `id` = ?" (Only bid)
  savedRoles :: [SavedRole] <- maybe [] fromOnly . only <$> queryLog "SELECT `roles` FROM `people` WHERE `id` = ?" (Only bid)
  hypixelRoles :: [HypixelRole] <- map fromOnly <$> queryLog "SELECT `hypixelRole` FROM `minecraft` JOIN `peopleMinecraft` ON `peopleMinecraft`.`minecraft` = `minecraft`.`uuid` WHERE `peopleMinecraft`.`id` = ? AND `hypixelRole` IS NOT NULL" (Only bid)
  for_ gmems $ \gmem -> do
    giveSavedRoles gmem savedRoles (Just hypixelRoles)
    giveRolesDivisionTitle gmem (foldl' max 0 wins)
    giveRolesMember gmem (not $ null hypixelRoles)
    removeIllegalRole gmem
applyRolesByBowBotId' Nothing gmems = for_ gmems $ \gmem -> do
  savedRoles :: [SavedRole] <- maybe [] fromOnly . only <$> queryLog "SELECT `roles` FROM `unregistered` WHERE `discord` = ?" (Only (maybe 0 userId (memberUser gmem)))
  giveSavedRoles gmem savedRoles Nothing
  giveRolesMemberUnregistered gmem
  giveIllegalRole gmem


applyRolesByBowBotId :: (MonadIOReader m r, HasAll '[Connection, DiscordHandle, Manager, CounterState, InfoCache] r) => BowBotId -> m ()
applyRolesByBowBotId bid = do
  gid <- askInfo discordGuildIdInfo
  gmems <- discordGuildMembers gid
  accountDiscords <- getDiscordIdsByBowBotId bid
  applyRolesByBowBotId' (Just bid) $ filter (\gmem -> maybe 0 userId (memberUser gmem) `elem` accountDiscords) gmems

applyRoles :: (MonadIOReader m r, HasAll '[Connection, DiscordHandle, Manager, CounterState, InfoCache] r) => GuildMember -> m ()
applyRoles gmem = do
  bid <- getBowBotIdByDiscord (maybe 0 userId (memberUser gmem))
  applyRolesByBowBotId' bid [gmem]

applyRolesAll :: (MonadIOReader m r, HasAll '[Connection, DiscordHandle, Manager, CounterState, InfoCache] r) => m ()
applyRolesAll = do
  lb <- getHypixelBowLeaderboards
  savedRoles :: M.Map UserId [SavedRole] <- M.fromList <$> queryLog "SELECT `discord`, `roles` FROM `unregistered` UNION SELECT `discord`, `roles` FROM `people` JOIN `peopleDiscord` ON `people`.`id` = `peopleDiscord`.`id`" ()
  gid <- askInfo discordGuildIdInfo
  gmems <- discordGuildMembers gid
  roles :: [(UUID, HypixelRole)] <- queryLog "SELECT `uuid`, `hypixelRole` FROM `minecraft` WHERE `hypixelRole` IS NOT NULL" ()
  accountMinecrafts :: M.Map UserId [UUID] <- M.map (map snd) . groupByToMap fst <$> queryLog "SELECT `peopleDiscord`.`discord`, `peopleMinecraft`.`minecraft` FROM `peopleMinecraft` JOIN `peopleDiscord` ON `peopleDiscord`.`id` = `peopleMinecraft`.`id`" ()
  for_ gmems $ \gmem -> do
    let discord = maybe 0 userId (memberUser gmem)
    case accountMinecrafts M.!? discord of
      Nothing -> do
        for_ (savedRoles M.!? discord) $ \r -> giveSavedRoles gmem r Nothing
        giveRolesMemberUnregistered gmem
        giveIllegalRole gmem
      Just mcs -> do
        let hypixelRoles = mapMaybe (`lookup` roles) mcs
        for_ (savedRoles M.!? discord) $ \r -> giveSavedRoles gmem r (Just hypixelRoles)
        giveRolesDivisionTitle gmem (foldl' max 0 (mapMaybe (fmap bowLbWins . (lb HM.!?)) mcs))
        giveRolesMember gmem (not $ null hypixelRoles)
        removeIllegalRole gmem