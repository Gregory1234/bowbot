{-# LANGUAGE QuasiQuotes #-}

module BowBot.Discord.SavedRoles where

import BowBot.Discord.Utils
import BowBot.BotData.Info
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Bifunctor (first)
import BowBot.DB.Basic
import BowBot.Account.Basic
import BowBot.Account.Register
import BowBot.Hypixel.Basic

toggleableRolesInfo :: InfoType (M.Map SavedRole RoleId)
toggleableRolesInfo = InfoType { infoName = "toggleable_roles", infoDefault = M.empty, infoParse = \s -> fmap M.fromList $ for (T.lines s) $ \l -> case T.splitOn "->" l of [a, b] -> (SavedRole a,) <$> fmap fromInteger ((first pack . readEither . unpack) b); _ -> Left "wrong format" }

savedRolesInfo :: InfoType (M.Map SavedRole RoleId)
savedRolesInfo = InfoType { infoName = "saved_roles", infoDefault = M.empty, infoParse = \s -> fmap M.fromList $ for (T.lines s) $ \l -> case T.splitOn "->" l of [a, b] -> (SavedRole a,) <$> fmap fromInteger ((first pack . readEither . unpack) b); _ -> Left "wrong format" }

savedHypixelRolesInfo :: InfoType (M.Map SavedRole ([HypixelRole], RoleId))
savedHypixelRolesInfo = InfoType { infoName = "hypixel_roles", infoDefault = M.empty, infoParse = \s -> fmap M.fromList $ for (T.lines s) $ \l -> case T.splitOn "->" l of [a, b, c] -> (SavedRole a,) . (map HypixelRole $ T.splitOn "|" b,) <$> fmap fromInteger ((first pack . readEither . unpack) c); _ -> Left "wrong format" }

newtype SavedRole = SavedRole { savedRoleName :: Text } deriving (Eq, Ord, Show)

instance Param [SavedRole]
instance Result [SavedRole]

deriving via (SimpleValue [SavedRole]) instance ToMysql [SavedRole]
deriving via (SimpleValue [SavedRole]) instance FromMysql [SavedRole]

instance ToField [SavedRole] where
  toField = T.encodeUtf8 . T.intercalate "," . map savedRoleName

instance FromField [SavedRole] where
  fromField = (textSqlTypes, Right . map SavedRole . filter (not . T.null) . T.splitOn "," . T.decodeUtf8)

savedRolesFromIds :: (MonadIOReader m r, Has InfoCache r) => [RoleId] -> m [SavedRole]
savedRolesFromIds roleids = do
  toggleableRoles <- askInfo toggleableRolesInfo
  savedRoles <- askInfo savedRolesInfo
  savedHypixelRoles <- askInfo savedHypixelRolesInfo
  return $ map fst $ filter ((`elem` roleids) . snd) $ M.toList toggleableRoles ++ M.toList savedRoles ++ map (\(a,(_,c)) -> (a,c)) (M.toList savedHypixelRoles)

-- TODO: switch the api to be BowBotId based, to not keep asking for user's BowBotId
setSavedRolesByDiscord :: (MonadIOReader m r, Has Connection r) => UserId -> [SavedRole] -> m ()
setSavedRolesByDiscord discord roles = do
  bid' <- if null roles then getBowBotIdByDiscord discord else getOrCreateDummyBowBotAccount discord -- NOTE: do not create an account if roles is empty
  for_ bid' $ \bid -> executeLog [mysql|INSERT INTO `account` (`id`, ^`roles`) VALUES (bid, roles)|]

updateSavedRolesAll :: (MonadIOReader m r, HasAll '[Connection, DiscordHandle, InfoCache] r) => m ()
updateSavedRolesAll = do
  savedRoles :: M.Map UserId [SavedRole] <- M.fromList <$> queryLog [mysql|SELECT `discord_id`, `roles` FROM `account` JOIN `account_discord` ON `account_id` = `account`.`id`|]
  gid <- askInfo discordGuildIdInfo
  members <- discordGuildMembers gid
  for_ members $ \GuildMember {..} -> case memberUser of
    Nothing -> return ()
    Just User {..} -> do
      roles <- savedRolesFromIds memberRoles
      when (roles /= fromMaybe [] (savedRoles M.!? userId)) $ setSavedRolesByDiscord userId roles

giveSavedRoles :: (MonadIOReader m r, HasAll '[Connection, DiscordHandle, InfoCache] r) => GuildMember -> [SavedRole] -> Maybe [HypixelRole] -> m ()
giveSavedRoles gmem roles hypixelRoles = do
  gid <- askInfo discordGuildIdInfo
  let partialSetUni roleMapFull roleMapPartial = addRemoveDiscordRoles gid gmem (map snd $ M.toList roleMapFull) (mapMaybe (roleMapPartial M.!?) roles)
  let partialSet roleMap = addRemoveDiscordRoles gid gmem (map snd $ M.toList roleMap) (mapMaybe (roleMap M.!?) roles)
  toggleableRoles <- askInfo toggleableRolesInfo
  savedRoles <- askInfo savedRolesInfo
  savedHypixelRoles <- askInfo savedHypixelRolesInfo
  partialSet toggleableRoles
  partialSet savedRoles
  case hypixelRoles of
    Nothing -> partialSet (M.map snd savedHypixelRoles)
    Just hypixelRoles' -> partialSetUni (M.map snd savedHypixelRoles) (M.map snd $ M.filter (not . null . intersect hypixelRoles' . fst) savedHypixelRoles)
