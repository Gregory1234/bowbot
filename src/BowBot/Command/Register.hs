{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module BowBot.Command.Register where

import BowBot.Command
import BowBot.Minecraft
import Data.Char (isDigit)
import Data.List (intercalate)
import BowBot.Background
import BowBot.Stats.HypixelBow

-- TODO: check if creation was successful

addMinecraftAccount :: DBMonad m => UUID -> [String] -> m (Maybe MinecraftAccount)
addMinecraftAccount uuid names = do
  count <- hExecuteLog "INSERT INTO `minecraftDEV`(`uuid`, `name`, `names`) VALUES (?,?,?)" (uuidString uuid, head names, intercalate "," names)
  return $ if count == 1
    then Just MinecraftAccount { mcUUID = uuid, mcNames = names, mcHypixelBow = Normal }
    else Nothing

addAccount :: (DBMonad m, MonadHoistIO m) => String -> UserId -> UUID -> m (Maybe BowBotAccount)
addAccount name did uuid = do
  res :: [Only Integer] <- hQueryLog "SELECT `id` FROM `discordDEV` WHERE `id`=?" (Only $ show did)
  if length res == 1
  then hTransaction $ do
    dt :: [(Maybe String, String)] <- hQueryLog "SELECT `birthday`, `roles` FROM `unregisteredDEV` WHERE `discord` = ?" (Only (show did))
    c1 <- case dt of
            [] -> hExecuteLog "INSERT INTO `peopleDEV`(`name`) VALUES (?)" (Only name)
            ((bd, roles):_) -> do
              _ <- hExecuteLog "DELETE FROM `unregisteredDEV` WHERE `discord` = ?" (Only $ show did)
              hExecuteLog "INSERT INTO `peopleDEV`(`name`,`birthday`,`roles`) VALUES (?,?,?)" (name, bd, roles)
    (fromIntegral -> aid) <- hInsertID
    c2 <- hExecuteLog "INSERT INTO `peopleMinecraftDEV`(`id`, `minecraft`,`status`, `selected`, `verified`) VALUES (?,?, 'main', 1, 0)" (aid, uuidString uuid)
    c3 <- hExecuteLog "INSERT INTO `peopleDiscordDEV`(`id`, `discord`) VALUES (?,?)" (aid, show did)
    assertIO (c1 == 1 && c2 == 1 && c3 == 1)
    return $ Just BowBotAccount { accountId = aid, accountDiscords = [did], accountMinecrafts = [uuid], accountSelectedMinecraft = uuid}
  else
    return Nothing

addAltAccount :: DBMonad m => Integer -> UUID -> m ()
addAltAccount gid (UUID uuid) = void $ hExecuteLog "INSERT INTO `peopleMinecraft$devornot`(`id`, `minecraft`,`status`, `selected`, `verified`) VALUES (?,?, 'alt', 0, 0)" (gid, uuid)

registerCommand :: String -> Bool -> Bool -> Command
registerCommand name isalt isself = Command name (if isself then DefaultLevel else ModLevel) 12 $ do
  hTryApiRequestsMulti [(hypixelRequestCounter, 2)] (\sec -> hRespond $ "**Too many requests! Wait another " ++ show sec ++ " seconds!**") $ do
    args <- hArgs
    caller <- hCaller
    when (null args) $ hRespond wrongSyntaxMessage
    let (did, mcname) = if isself then (userId caller, head args) else (read . filter isDigit $ head args, args !! 1)
    discords <- getDiscordIds
    if did `notElem` discords
    then hRespond "*The discord id doesn't exist!*"
    else do
      maybeUUID <- mcNameToUUID mcname
      case maybeUUID of
        Nothing -> hRespond playerNotFoundMessage
        Just uuid -> do
          nicks <- hRead minecraftAccounts
          taken <- (>>=accountMinecrafts) <$> hRead bowBotAccounts
          if uuid `elem` taken
          then do
            pns <- (>>=(\x -> (, x) <$> accountDiscords x)) <$> hRead bowBotAccounts
            case lookup did pns of
              Nothing -> hRespond "*That account already belongs to someone else!*"
              Just bac -> hRespond $ if uuid `elem` accountMinecrafts bac
                then (if isself then "*That account already belongs to you!*" else "*That account already belongs to this user!*")
                else "*That account already belongs to someone else!*"
          else do
            names <- fromMaybe [] <$> mcUUIDToNames uuid
            unless (uuid `elem` map mcUUID nicks) $ do
              newMc <- addMinecraftAccount uuid names
              for_ newMc $ \x -> hWrite minecraftAccounts (x:nicks)
            if isalt
            then do
              pns <- (>>=(\BowBotAccount {..} -> (, accountId) <$> accountDiscords)) <$> hRead bowBotAccounts
              case lookup did pns of
                Nothing -> hRespond (if isself then "*You are not registered!*" else "*That person is not registered!*")
                Just gid -> do
                  addAltAccount gid uuid
                  psa <- hRead bowBotAccounts
                  let oldAcc = head $ filter ((==gid) . accountId) psa
                  hWrite bowBotAccounts (oldAcc { accountMinecrafts = uuid:accountMinecrafts oldAcc } : filter ((/= gid) . accountId) psa)
                  fullUpdateHypixelBowStats uuid
                  updateDiscordRolesSingleId did
                  hRespond "*Registered successfully*"
            else do
              pns <- (>>=(\BowBotAccount {..} -> (, accountId) <$> accountDiscords)) <$> hRead bowBotAccounts
              case lookup did pns of
                Nothing -> do
                  newAcc <- addAccount (head names) did uuid
                  case newAcc of
                    Nothing -> hRespond somethingWrongMessage
                    Just newAcc' -> do
                      psa <- hRead bowBotAccounts
                      hWrite bowBotAccounts (newAcc':psa)
                      fullUpdateHypixelBowStats uuid
                      updateDiscordRolesSingleId did
                      hRespond "*Registered successfully*"
                Just _ -> hRespond (if isself then "*You are already registered!*" else "*That person is already registered!*")