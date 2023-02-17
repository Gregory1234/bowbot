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
import qualified Discord.Requests as R
import BowBot.DB.Basic (queryLog, withDB, executeManyLog', Connection, Only(..))
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

giveRolesDivisionTitle :: (MonadIOBotData m d r, HasCache InfoField d, HasAll '[Connection, DiscordHandle] r) => GuildMember -> Integer -> m ()
giveRolesDivisionTitle gmem maxWins = do
  gid <- askInfo discordGuildIdInfo
  allRoles <- askInfo divisionTitleRolesInfo
  let selectedRole = map snd $ take 1 $ reverse $ takeWhile ((<= maxWins) . fst) allRoles
  addRemoveDiscordRoles gid gmem (map snd allRoles) selectedRole

applyRolesDivisionTitleByBowBotAccount' :: (MonadIOBotData m d r, HasCache InfoField d, HasAll '[Connection, DiscordHandle] r) => BowBotAccount -> [GuildMember] -> m ()
applyRolesDivisionTitleByBowBotAccount' bbacc gmems = do
  wins :: [Integer] <- map fromOnly <$> queryLog "SELECT `bowWins` FROM `stats` JOIN `peopleMinecraft` ON `stats`.`minecraft` = `peopleMinecraft`.`minecraft` WHERE `id` = ?" (Only (accountBotId bbacc))
  for_ gmems $ \gmem -> do
    giveRolesDivisionTitle gmem (foldl' max 0 wins)

applyRolesDivisionTitleByBowBotAccount :: (MonadIOBotData m d r, HasCache InfoField d, HasAll '[Connection, DiscordHandle] r) => BowBotAccount -> m ()
applyRolesDivisionTitleByBowBotAccount bbacc = do
  gid <- askInfo discordGuildIdInfo
  gmems <- discordGuildMembers gid
  applyRolesDivisionTitleByBowBotAccount' bbacc $ filter (\gmem -> maybe 0 userId (memberUser gmem) `elem` accountDiscords bbacc) gmems

applyRolesDivisionTitleByUUID :: (MonadIOBotData m d r, HasCaches '[InfoField, BowBotAccount] d, HasAll '[Connection, DiscordHandle] r) => UUID -> m ()
applyRolesDivisionTitleByUUID uuid = do
  bbacc <- getBowBotAccountByMinecraft uuid
  for_ bbacc applyRolesDivisionTitleByBowBotAccount

giveRolesMember :: (MonadIOBotData m d r, HasCache InfoField d, Has DiscordHandle r) => GuildMember -> Bool -> m ()
giveRolesMember gmem isMember = do
  memberRole <- askInfo memberRoleInfo
  visitorRole <- askInfo visitorRoleInfo
  let expectedRole = if isMember then memberRole else visitorRole
  gid <- askInfo discordGuildIdInfo
  addRemoveDiscordRoles gid gmem [memberRole, visitorRole] [expectedRole]

giveRolesMemberUnregistered :: (MonadIOBotData m d r, HasCache InfoField d, Has DiscordHandle r) => GuildMember -> m ()
giveRolesMemberUnregistered gmem = do
  memberRole <- askInfo memberRoleInfo
  visitorRole <- askInfo visitorRoleInfo
  gid <- askInfo discordGuildIdInfo
  unless (memberRole `elem` memberRoles gmem) $ addRemoveDiscordRoles gid gmem [visitorRole] [visitorRole]

removeIllegalRole :: (MonadIOBotData m d r, HasCache InfoField d, Has DiscordHandle r) => GuildMember -> m ()
removeIllegalRole gmem = do
  illegalRole <- askInfo illegalRoleInfo
  gid <- askInfo discordGuildIdInfo
  addRemoveDiscordRoles gid gmem [illegalRole] []

giveIllegalRole :: (MonadIOBotData m d r, HasCache InfoField d, Has DiscordHandle r) => GuildMember -> m ()
giveIllegalRole gmem = do
  memberRole <- askInfo memberRoleInfo
  savedHypixelRoles <- map (snd . snd) . M.toList <$> askInfo savedHypixelRolesInfo
  divisionTitleRoles <- map snd <$> askInfo divisionTitleRolesInfo
  illegalRole <- askInfo illegalRoleInfo
  gid <- askInfo discordGuildIdInfo
  addRemoveDiscordRoles gid gmem [illegalRole] $ [illegalRole | not . null $ intersect (memberRoles gmem) (memberRole:savedHypixelRoles ++ divisionTitleRoles)]

applyRolesByBowBotAccount' :: (MonadIOBotData m d r, HasCaches '[InfoField, BowBotAccount] d, HasAll '[Connection, DiscordHandle, Manager, CounterState] r) => Maybe BowBotAccount -> [GuildMember] -> m ()
applyRolesByBowBotAccount' (Just bbacc) gmems = do
  wins :: [Integer] <- map fromOnly <$> queryLog "SELECT `bowWins` FROM `stats` JOIN `peopleMinecraft` ON `stats`.`minecraft` = `peopleMinecraft`.`minecraft` WHERE `id` = ?" (Only (accountBotId bbacc))
  savedRoles :: [SavedRole] <- maybe [] fromOnly . only <$> queryLog "SELECT `roles` FROM `people` WHERE `id` = ?" (Only (accountBotId bbacc))
  hypixelRoles :: [HypixelRole] <- map fromOnly <$> queryLog "SELECT `uuid`, `hypixelRole` FROM `minecraft` WHERE `uuid` IN ? AND `hypixelRole` IS NOT NULL" (Only (In (accountMinecrafts bbacc)))
  for_ gmems $ \gmem -> do
    giveSavedRoles gmem savedRoles (Just hypixelRoles)
    giveRolesDivisionTitle gmem (foldl' max 0 wins)
    giveRolesMember gmem (not $ null hypixelRoles)
    removeIllegalRole gmem
applyRolesByBowBotAccount' Nothing gmems = for_ gmems $ \gmem -> do
  savedRoles :: [SavedRole] <- maybe [] fromOnly . only <$> queryLog "SELECT `roles` FROM `unregistered` WHERE `discord` = ?" (Only (maybe 0 userId (memberUser gmem)))
  giveSavedRoles gmem savedRoles Nothing
  giveRolesMemberUnregistered gmem
  giveIllegalRole gmem


applyRolesByBowBotAccount :: (MonadIOBotData m d r, HasCaches '[InfoField, BowBotAccount] d, HasAll '[Connection, DiscordHandle, Manager, CounterState] r) => BowBotAccount -> m ()
applyRolesByBowBotAccount bbacc = do
  gid <- askInfo discordGuildIdInfo
  gmems <- discordGuildMembers gid
  applyRolesByBowBotAccount' (Just bbacc) $ filter (\gmem -> maybe 0 userId (memberUser gmem) `elem` accountDiscords bbacc) gmems

applyRoles :: (MonadIOBotData m d r, HasCaches '[InfoField, BowBotAccount] d, HasAll '[Connection, DiscordHandle, Manager, CounterState] r) => GuildMember -> m ()
applyRoles gmem = do
  bbacc <- getBowBotAccountByDiscord (maybe 0 userId (memberUser gmem))
  applyRolesByBowBotAccount' bbacc [gmem]

applyRolesAll :: (MonadIOBotData m d r, HasCaches '[InfoField, BowBotAccount] d, HasAll '[Connection, DiscordHandle, Manager, CounterState] r) => m ()
applyRolesAll = do
  lb <- getHypixelBowLeaderboards
  savedRoles :: M.Map UserId [SavedRole] <- M.fromList <$> queryLog "SELECT `discord`, `roles` FROM `unregistered` UNION SELECT `discord`, `roles` FROM `people` JOIN `peopleDiscord` ON `people`.`id` = `peopleDiscord`.`id`" ()
  gid <- askInfo discordGuildIdInfo
  gmems <- discordGuildMembers gid
  roles :: [(UUID, HypixelRole)] <- queryLog "SELECT `uuid`, `hypixelRole` FROM `minecraft` WHERE `hypixelRole` IS NOT NULL" ()
  for_ gmems $ \gmem -> do
    let discord = maybe 0 userId (memberUser gmem)
    bbacc <- getBowBotAccountByDiscord discord
    case bbacc of
      Nothing -> do
        giveSavedRoles gmem (savedRoles M.! discord) Nothing
        giveRolesMemberUnregistered gmem
        giveIllegalRole gmem
      Just acc -> do
        let hypixelRoles = mapMaybe (`lookup` roles) (accountMinecrafts acc)
        giveSavedRoles gmem (savedRoles M.! discord) (Just hypixelRoles)
        giveRolesDivisionTitle gmem (foldl' max 0 (mapMaybe (fmap bowLbWins . (lb HM.!?)) (accountMinecrafts acc)))
        giveRolesMember gmem (not $ null hypixelRoles)
        removeIllegalRole gmem