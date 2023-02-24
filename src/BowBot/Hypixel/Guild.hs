module BowBot.Hypixel.Guild where

import BowBot.BotData.Info
import BowBot.Minecraft.Basic
import qualified Data.HashMap.Strict as HM
import BowBot.BotData.Cached
import BowBot.Counter.Basic
import BowBot.Network.Basic hiding (Result)
import BowBot.Hypixel.Basic
import BowBot.Discord.Utils
import BowBot.Minecraft.Account
import BowBot.DB.Basic
import Database.MySQL.Simple (Param, Result)


hypixelGuildIdInfo :: InfoType Text
hypixelGuildIdInfo = InfoType { infoName = "hypixel_guild_id", infoDefault = "", infoParse = Right }

newtype HypixelRole = HypixelRole { fromHypixelRole :: Text }
  deriving (Eq, Ord, Show)
  deriving newtype (Param, Result)

updateHypixelRoles :: (MonadHoistIOBotData m d r, HasAll '[Manager, CounterState, Connection] r, HasCaches '[InfoField, MinecraftAccount] d) => m ()
updateHypixelRoles = do
  cv <- tryIncreaseCounter HypixelApi 1
  case cv of
    Nothing -> do
      gid <- askInfo hypixelGuildIdInfo
      members' <- hypixelGuildMemberList gid
      case members' of
        Nothing -> return ()
        Just members -> do
          known <- HM.keys <$> getCacheMap @MinecraftAccount
          let unknown = map fst members \\ known
          names <- catMaybes <$> traverse (\x -> fmap (x,) <$> mojangUUIDToCurrentName x) unknown
          b <- storeInCache [MinecraftAccount {mcUUID, mcNames = [mcName]} | (mcUUID, mcName) <- names]
          when b $ withTransaction $ void $ executeManyLog "INSERT INTO `minecraft` (`uuid`, `hypixelRole`) VALUES (?,?) ON DUPLICATE KEY UPDATE `hypixelRole`=VALUES(`hypixelRole`)" members
    _ -> return ()

getHypixelRoleByUUID :: (MonadIOReader m r, HasAll '[Manager, CounterState, Connection] r) => UUID -> m (Maybe HypixelRole)
getHypixelRoleByUUID uuid = join . only . map fromOnly <$> queryLog "SELECT `hypixelRole` FROM `minecraft` WHERE `uuid` = ?" (Only uuid)

getHypixelGuildMembers :: (MonadIOReader m r, HasAll '[Manager, CounterState, Connection] r) => m [UUID]
getHypixelGuildMembers = map fromOnly <$> queryLog "SELECT `uuid` FROM `minecraft` WHERE `hypixelRole` IS NOT NULL" ()