{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module BowBot.Command.Register where

import BowBot.Command
import BowBot.Minecraft
import Data.Char (isDigit)
import Data.List (intercalate)
import BowBot.Background
import BowBot.Stats.HypixelBow

-- TODO: check if creation was successful

addMinecraftAccount :: APIMonad m => String -> [String] -> m (Maybe MinecraftAccount)
addMinecraftAccount uuid names = do
  _ <- hSendDB "minecraft/new.php" ["uuid=" ++ uuid, "names=" ++ intercalate "," names]
  return $ Just MinecraftAccount { mcUUID = uuid, mcNames = names, mcHypixelBow = Normal }

addAccount :: APIMonad m => String -> UserId -> String -> m (Maybe BowBotAccount)
addAccount name did uuid = do
  res <- hSendDB "people/new.php" ["name=" ++ name, "discord=" ++ show did, "verified=0", "minecraft=" ++ uuid]
  decodeParse res $ \o -> do
    (readMaybe -> Just aid) <- o .: "id"
    return BowBotAccount { accountId = aid, accountDiscords = [did], accountMinecrafts = [uuid], accountSelectedMinecraft = uuid}

addAltAccount :: APIMonad m => Integer -> String -> m ()
addAltAccount gid uuid = do
  _ <- hSendDB "people/alt.php" ["id=" ++ show gid, "verified=0", "minecraft=" ++ uuid]
  return ()

registerCommand :: String -> Bool -> Bool -> Command
registerCommand name isalt isself = Command name (if isself then DefaultLevel else ModLevel) 12 $ do
  bdt <- hData
  tryApiRequestsMulti [(hypixelRequestCounter bdt, 2)] (\sec -> hRespond $ "**Too many requests! Wait another " ++ show sec ++ " seconds!**") $ do
    args <- hArgs
    caller <- hCaller
    when (null args) $ hRespond wrongSyntaxMessage
    let (did, mcname) = if isself then (userId caller, head args) else (read . filter isDigit $ head args, args !! 1)
    discords <- getDiscordIds
    if did `notElem` discords
    then hRespond "*The discord id doesn't exist!*"
    else do
      maybeUUID <- mcNameToUUID bdt mcname
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
            names <- fromMaybe [] <$> mcUUIDToNames bdt uuid
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
                  hMDiscord $ updateDiscordRolesSingleId bdt did
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
                      hMDiscord $ updateDiscordRolesSingleId bdt did
                      hRespond "*Registered successfully*"
                Just _ -> hRespond (if isself then "*You are already registered!*" else "*That person is already registered!*")