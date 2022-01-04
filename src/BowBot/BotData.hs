{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

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

downloadMinecraftAccounts :: APIMonad m => m (Maybe [MinecraftAccount])
downloadMinecraftAccounts = do
  res <- hSendDB "minecraft/all.php" []
  decodeParse res $ \o -> do
    dt <- o .: "data"
    for dt $ \acc -> do
      mcUUID <- acc .: "uuid"
      mcNames <- acc .: "names"
      (stringToUpdateFreq -> Just mcHypixelBow) <- acc .: "hypixel"
      return MinecraftAccount {..}


downloadBowBotAccounts :: APIMonad m => m (Maybe [BowBotAccount])
downloadBowBotAccounts = do
  res <- hSendDB "people/all.php" []
  decodeParse res $ \o -> do
    dt <- o .: "data"
    for (toList dt) $ \(accountId, acc) -> do
      accountDiscords <- acc .: "discord"
      accountSelectedMinecraft <- acc .: "selected"
      accountMinecrafts <- acc .: "minecraft"
      return BowBotAccount {..}

downloadDiscordPerms :: APIMonad m => m (Maybe (Map UserId PermissionLevel))
downloadDiscordPerms = do
  res <- hSendDB "discord/perms.php" []
  decodeParse res $ \o -> do
    dt <- o .: "data"
    fmap fromList . for dt $ \dp -> do
      did <- dp .: "id"
      (stringToPermissionLevel -> Just level) <- dp .: "level"
      return (did, level)

downloadData :: BotData -> IO ()
downloadData bdt = do
  manager <- newManager managerSettings
  newMinecraftAccounts <- runManagerT downloadMinecraftAccounts manager
  newDiscordSettings <- runManagerT getSettings manager
  newBowBotAccounts <- runManagerT downloadBowBotAccounts manager
  newDiscordPerms <- runManagerT downloadDiscordPerms manager
  atomically $ do
    for_ newMinecraftAccounts (writeTVar (minecraftAccounts bdt))
    for_ newDiscordSettings (writeTVar (discordSettings bdt))
    for_ newBowBotAccounts (writeTVar (bowBotAccounts bdt))
    for_ newDiscordPerms (writeTVar (discordPerms bdt))
  withDB $ updateDiscordConstants bdt
  tryApiRequests (hypixelRequestCounter bdt) 1 (\_ -> pure ()) $ do
    gid <- readProp hypixelGuildId bdt
    newGuildMembers <- runManagerT (hypixelGuildMemberList gid) manager
    atomically $ for_ newGuildMembers (writeTVar (hypixelGuildMembers bdt))
    

updateMinecraftAccounts :: APIMonad m => BotData -> m ()
updateMinecraftAccounts bdt = do
  manager <- hManager
  nickList <- readProp minecraftAccounts bdt
  let chunked = chunksOf 10 nickList
  updatedNicks <- liftIO $ fmap concat $ for chunked $ mapConcurrently (fmap (`runManagerT` manager) helper)
  writeProp minecraftAccounts bdt updatedNicks
  where
    helper MinecraftAccount {..} = do
      newNames <- mojangUUIDToNames mcUUID
      for_ newNames $ \names ->
        unless (mcNames == names) $ updateMinecraftNames mcUUID names
      return MinecraftAccount {mcNames = fromMaybe mcNames newNames, ..}
    updateMinecraftNames uuid names = 
      void $ hSendDB "minecraft/setnames.php" ["uuid=" ++ uuid, "names=" ++ intercalate "," names]

updateDiscordConstants :: BotData -> Connection -> IO ()
updateDiscordConstants bdt conn = do
  let getSnowflake name = (>>= readMaybe @Snowflake) <$> getInfoDB conn name
  newHypixelGuildId <- getInfoDB conn "hypixel_guild_id"
  newDiscordGuildId <- getSnowflake "discord_guild_id"
  newDiscordIllegalRole <- getSnowflake "illegal_role"
  newDiscordMemberRole <- getSnowflake "member_role"
  newDiscordVisitorRole <- getSnowflake "visitor_role"
  let parseDivisionRole s = case splitOn "->" s of
        [readMaybe -> Just wins, readMaybe -> Just role] -> Just (wins, role)
        _ -> Nothing
  let parseDivisionRoles s = traverse parseDivisionRole $ lines s
  newDiscordDivisionRoles <- (>>= parseDivisionRoles) <$> getInfoDB conn "division_title_roles"
  let parseToggleableRole s = case splitOn "->" s of
        [name, readMaybe -> Just role] -> Just (name, role)
        _ -> Nothing
  let parseToggleableRoles s = traverse parseToggleableRole $ lines s
  newDiscordToggleableRoles <- (>>= parseToggleableRoles) <$> getInfoDB conn "toggleable_roles"
  newDiscordCommandPrefix <- getInfoDB conn "command_prefix"
  newDiscordBirthdayChannel <- getSnowflake "birthday_channel"
  atomically $ do
    for_ newHypixelGuildId (writeTVar (hypixelGuildId bdt))
    for_ newDiscordGuildId (writeTVar (discordGuildId bdt))
    for_ newDiscordIllegalRole (writeTVar (discordIllegalRole bdt))
    for_ newDiscordMemberRole (writeTVar (discordMemberRole bdt))
    for_ newDiscordVisitorRole (writeTVar (discordVisitorRole bdt))
    for_ newDiscordDivisionRoles (writeTVar (discordDivisionRoles bdt))
    for_ newDiscordToggleableRoles (writeTVar (discordToggleableRoles bdt))
    for_ newDiscordCommandPrefix (writeTVar (discordCommandPrefix bdt))
    for_ newDiscordBirthdayChannel (writeTVar (discordBirthdayChannel bdt))


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
  discordCommandPrefix <- newTVar "???"
  discordBirthdayChannel <- newTVar 0
  return BotData {..}

createData :: IO BotData
createData = do
  bdt <- atomically emptyData
  downloadData bdt
  return bdt