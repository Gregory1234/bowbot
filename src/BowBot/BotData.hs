{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module BowBot.BotData(
  module BowBot.BotData, module BowBot.BotData.Core
) where

import Control.Concurrent.STM.TVar (newTVar)
import Data.Map (Map, empty, toList, fromList)
import Discord.Types
import BowBot.API
import BowBot.Settings
import Control.Concurrent.STM (STM)
import Network.HTTP.Conduit (newManager)
import Data.List.Split (chunksOf, splitOn)
import Control.Concurrent.Async (mapConcurrently)
import BowBot.API.Mojang (mojangUUIDToNames)
import Data.List (intercalate)
import BowBot.API.Hypixel
import BowBot.DB
import BowBot.BotData.Core
import BowBot.CommandMonads

downloadMinecraftAccounts :: DBMonad m => m [MinecraftAccount]
downloadMinecraftAccounts = do
  res :: [(String, String, String)] <- hQueryLog "SELECT `uuid`, `names`, `hypixel` FROM `minecraftDEV`" ()
  return $ flip fmap res $ \case
    (UUID -> mcUUID, splitOn "," -> mcNames, stringToUpdateFreq -> Just mcHypixelBow) -> MinecraftAccount {..}
    (UUID -> mcUUID, splitOn "," -> mcNames, _) -> MinecraftAccount {mcHypixelBow = Normal, ..}


downloadBowBotAccounts :: DBMonad m => m [BowBotAccount]
downloadBowBotAccounts = do
  minecrafts :: [(Integer, String, Bool)] <- hQueryLog "SELECT `id`, `minecraft`, `selected` FROM `peopleMinecraftDEV`" ()
  discords :: [(Integer, Integer)] <- hQueryLog "SELECT `id`, `discord` FROM `peopleDiscordDEV`" ()
  return $ map helper $ toList $ zipMapWith join (groupToMap $ fmap (\(a,b,c) -> (a,(b,c))) minecrafts) (groupToMap discords) -- TODO: probably exists a better way
    where
      helper (accountId,(minecrafts, discords)) = 
        let accountMinecrafts = map (UUID . fst) minecrafts
            accountSelectedMinecraft = UUID . fst . head $ filter snd minecrafts
            accountDiscords = map fromInteger discords
        in BowBotAccount {..}
      join (fromMaybe [] -> mcs) (fromMaybe [] -> ds) = (mcs, ds)

downloadDiscordPerms :: DBMonad m => m (Map UserId PermissionLevel)
downloadDiscordPerms = do
  res :: [(Integer, String)] <- hQueryLog "SELECT `id`, `level` FROM `permissionsDEV`" ()
  return $ fromList $ flip fmap res $ \case
    (fromInteger -> discord, stringToPermissionLevel -> Just level) -> (discord, level)
    (fromInteger -> discord, _) -> (discord, DefaultLevel)

downloadData :: (BotDataMonad m, APIMonad m, DBMonad m) => m ()
downloadData = do
  newMinecraftAccounts <- downloadMinecraftAccounts
  newDiscordSettings <- getSettings
  newBowBotAccounts <- downloadBowBotAccounts
  newDiscordPerms <- downloadDiscordPerms
  liftIO $ print newBowBotAccounts
  hWrite minecraftAccounts newMinecraftAccounts
  hWrite discordSettings newDiscordSettings
  hWrite bowBotAccounts newBowBotAccounts
  hWrite discordPerms newDiscordPerms
  withDB $ runConnectionT updateDiscordConstants
  hTryApiRequests hypixelRequestCounter 1 (\_ -> pure ()) $ do
    gid <- hRead hypixelGuildId
    newGuildMembers <- hypixelGuildMemberList gid
    for_ newGuildMembers (hWrite hypixelGuildMembers)
    

updateMinecraftAccounts :: (DBMonad m, APIMonad m) => BotData -> m ()
updateMinecraftAccounts bdt = do
  manager <- hManager
  conn <- hConnection
  nickList <- readProp minecraftAccounts bdt
  let chunked = chunksOf 10 nickList
  updatedNicks <- liftIO $ fmap concat $ for chunked $ mapConcurrently (fmap ((`runConnectionT` conn) . (`runManagerT` manager)) helper)
  writeProp minecraftAccounts bdt updatedNicks
  where
    helper MinecraftAccount {..} = do
      newNames <- mojangUUIDToNames mcUUID
      for_ newNames $ \names ->
        unless (mcNames == names) $ updateMinecraftNames mcUUID names
      return MinecraftAccount {mcNames = fromMaybe mcNames newNames, ..}
    updateMinecraftNames (UUID uuid) names = 
      void $ hExecuteLog "UPDATE `minecraftDEV` SET `names`=?, `name`=? WHERE `uuid`=?" (intercalate "," names, head names, uuid)

updateDiscordConstants :: (BotDataMonad m, DBMonad m) => m ()
updateDiscordConstants = do
  let getSnowflake name = (>>= readMaybe @Snowflake) <$> hInfoDB name
  newHypixelGuildId <- hInfoDB "hypixel_guild_id"
  newDiscordGuildId <- getSnowflake "discord_guild_id"
  newDiscordIllegalRole <- getSnowflake "illegal_role"
  newDiscordMemberRole <- getSnowflake "member_role"
  newDiscordVisitorRole <- getSnowflake "visitor_role"
  let parseDivisionRole s = case splitOn "->" s of
        [readMaybe -> Just wins, readMaybe -> Just role] -> Just (wins, role)
        _ -> Nothing
  let parseDivisionRoles s = traverse parseDivisionRole $ lines s
  newDiscordDivisionRoles <- (>>= parseDivisionRoles) <$> hInfoDB "division_title_roles"
  let parseNamedRole s = case splitOn "->" s of
        [name, readMaybe -> Just role] -> Just (name, role)
        _ -> Nothing
  let parseNamedRoles s = traverse parseNamedRole $ lines s
  newDiscordToggleableRoles <- (>>= parseNamedRoles) <$> hInfoDB "toggleable_roles"
  newDiscordOtherSavedRoles <- (>>= parseNamedRoles) <$> hInfoDB "saved_roles"
  newDiscordCommandPrefix <- hInfoDB "command_prefix"
  newDiscordBirthdayChannel <- getSnowflake "birthday_channel"
  for_ newHypixelGuildId (hWrite hypixelGuildId)
  for_ newDiscordGuildId (hWrite discordGuildId)
  for_ newDiscordIllegalRole (hWrite discordIllegalRole)
  for_ newDiscordMemberRole (hWrite discordMemberRole)
  for_ newDiscordVisitorRole (hWrite discordVisitorRole)
  for_ newDiscordDivisionRoles (hWrite discordDivisionRoles)
  for_ newDiscordToggleableRoles (hWrite discordToggleableRoles)
  for_ newDiscordOtherSavedRoles (hWrite discordOtherSavedRoles)
  for_ newDiscordCommandPrefix (hWrite discordCommandPrefix)
  for_ newDiscordBirthdayChannel (hWrite discordBirthdayChannel)


newRequestCounter :: Int -> STM ApiRequestCounter
newRequestCounter counterLimit = do
  mainCounter <- newTVar 0
  borderCounter <- newTVar 0
  return ApiRequestCounter {..}

newCachedData :: STM (CachedData a)
newCachedData = do
  mainCache <- newTVar Nothing
  borderCache <- newTVar Nothing
  currentlyBusyCache <- newTVar False
  return CachedData {..}

emptyData :: STM BotData
emptyData = do
  hypixelRequestCounter <- newRequestCounter 100
  minecraftAccounts <- newTVar []
  hypixelBowOnlineList <- newCachedData
  discordSettings <- newTVar empty
  bowBotAccounts <- newTVar []
  discordPerms <- newTVar empty
  hypixelGuildMembers <- newTVar []
  snipeMessage <- newTVar empty
  hypixelGuildId <- newTVar ""
  discordGuildId <- newTVar 0
  discordIllegalRole <- newTVar 0
  discordMemberRole <- newTVar 0
  discordVisitorRole <- newTVar 0
  discordDivisionRoles <- newTVar []
  discordToggleableRoles <- newTVar []
  discordOtherSavedRoles <- newTVar []
  discordCommandPrefix <- newTVar "???"
  discordBirthdayChannel <- newTVar 0
  return BotData {..}

createData :: IO BotData
createData = do
  bdt <- atomically emptyData
  man <- newManager managerSettings
  withDB $ runConnectionT $ runManagerT (runBotDataT downloadData bdt) man
  return bdt