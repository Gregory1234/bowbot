{-# LANGUAGE QuasiQuotes #-}

module BowBot.Hypixel.Guild where

import BowBot.BotData.Info
import BowBot.Minecraft.Basic
import BowBot.Counter.Basic
import BowBot.Network.Basic hiding (Result)
import BowBot.Hypixel.Basic
import BowBot.Discord.Utils
import BowBot.Minecraft.Account
import BowBot.DB.Typed


hypixelGuildIdInfo :: InfoType Text
hypixelGuildIdInfo = InfoType { infoName = "hypixel_guild_id", infoDefault = "", infoParse = Right }

updateHypixelRoles :: (MonadHoistIOReader m r, HasAll '[InfoCache, Manager, CounterState, Connection] r) => m ()
updateHypixelRoles = do
  cv <- tryIncreaseCounter HypixelApi 1
  case cv of
    Nothing -> do
      gid <- askInfo hypixelGuildIdInfo
      members' <- hypixelGuildMemberList gid
      case members' of
        Nothing -> return ()
        Just members -> do
          known <- queryLogT [mysql|SELECT `uuid` FROM `minecraft`|]
          let unknown = map fst members \\ known
          names <- catMaybes <$> traverse (\x -> fmap (x,) <$> mojangUUIDToCurrentName x) unknown
          let toInsert = [MinecraftAccount {mcUUID, mcNames = [mcName, mcName <> "OldNamesCurrentlyNotKnown"]} | (mcUUID, mcName) <- names]
          b <- (>0) <$> executeLogT [mysql|INSERT INTO `minecraft`(MinecraftAccount) VALUES toInsert..|]
          c <- addMinecraftNames (map (\(u,n) -> (n,u)) names)
          when ((b && c) || null unknown) $ void $ executeLogT [mysql|INSERT INTO `minecraft` (`uuid`, ^`hypixel_role`) VALUES members..|]
          let memberUUIDs = map fst members
          void $ executeLogT [mysql|UPDATE `minecraft` SET `hypixel_role` = NULL WHERE `uuid` NOT IN memberUUIDs|]
    _ -> return ()

getHypixelGuildMembers :: (MonadIOReader m r, HasAll '[Manager, CounterState, Connection] r) => m [UUID]
getHypixelGuildMembers = queryLogT [mysql|SELECT `uuid` FROM `minecraft` WHERE `hypixel_role` IS NOT NULL|]