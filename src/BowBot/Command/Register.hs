{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module BowBot.Command.Register where

import BowBot.Command
import BowBot.Minecraft
import BowBot.Stats
import BowBot.API
import Data.Char (isSpace, isDigit)
import Data.List (intercalate)
import BowBot.Background

-- TODO: check if creation was successful

addMinecraftAccount :: Manager -> String -> [String] -> IO (Maybe MinecraftAccount)
addMinecraftAccount manager uuid names = do
  _ <- sendDB manager "minecraft/new.php" ["uuid=" ++ uuid, "names=" ++ intercalate "," names]
  return $ Just MinecraftAccount { mcUUID = uuid, mcNames = names, mcHypixelBow = Daily }

addAccount :: Manager -> String -> UserId -> String -> IO (Maybe BowBotAccount)
addAccount manager name did uuid = do
  res <- sendDB manager "people/new.php" ["name=" ++ name, "discord=" ++ show did, "verified=0", "minecraft=" ++ uuid]
  decodeParse res $ \o -> do
    (readMaybe -> Just aid) <- o .: "id"
    return BowBotAccount { accountId = aid, accountDiscords = [did], accountMinecrafts = [uuid], accountSelectedMinecraft = uuid}

addAltAccount :: Manager -> Integer -> String -> IO ()
addAltAccount manager gid uuid = do
  _ <- sendDB manager "people/alt.php" ["id=" ++ show gid, "verified=0", "minecraft=" ++ uuid]
  return ()

registerCommand :: String -> [BotData -> ApiRequestCounter] -> Bool -> Bool -> (Manager -> String -> IO ()) -> Command
registerCommand name apis isalt isself onComplete = Command name (if isself then DefaultLevel else ModLevel) 12 $ \m man bdt -> do
  tryApiRequestsMulti (map (\x -> (x bdt, 2)) apis) (\sec -> respond m $ "**Too many requests! Wait another " ++ show sec ++ " seconds!**") $ do
    let args = words $ dropWhile isSpace $ dropWhile (not . isSpace) $ unpack (messageText m)
    when (null args) $ respond m wrongSyntaxMessage
    let (did, mcname) = if isself then (userId $ messageAuthor m, head args) else (read . filter isDigit $ head args, args !! 1)
    discords <- liftIO $ getDiscordIds man
    if did `notElem` discords
    then do
      respond m "*The discord id doesn't exist!*"
    else do
      maybeUUID <- liftIO $ mcNameToUUID man bdt mcname
      case maybeUUID of
        Nothing -> respond m playerNotFoundMessage
        Just uuid -> do
          nicks <- liftIO $ atomically $ readTVar (minecraftAccounts bdt)
          taken <- fmap (>>=accountMinecrafts) $ liftIO $ atomically $ readTVar (bowBotAccounts bdt)
          if uuid `elem` taken
          then do
            pns <- fmap (>>=(\x -> (, x) <$> accountDiscords x)) $ liftIO $ atomically $ readTVar (bowBotAccounts bdt)
            case lookup did pns of
              Nothing -> respond m "*That account already belongs to someone else!*"
              Just bac -> respond m $ if uuid `elem` accountMinecrafts bac
                then (if isself then "*That account already belongs to you!*" else "*That account already belongs to this user!*")
                else "*That account already belongs to someone else!*"
          else do
            names <- liftIO $ fromMaybe [] <$> mcUUIDToNames man bdt uuid
            unless (uuid `elem` map mcUUID nicks) $ do
              newMc <- liftIO $ addMinecraftAccount man uuid names
              for_ newMc $ \x -> liftIO $ atomically $ writeTVar (minecraftAccounts bdt) (x:nicks)
            if isalt
            then do
              pns <- fmap (>>=(\BowBotAccount {..} -> (, accountId) <$> accountDiscords)) $ liftIO $ atomically $ readTVar (bowBotAccounts bdt)
              case lookup did pns of
                Nothing -> respond m (if isself then "*You are not registered!*" else "*That person is not registered!*")
                Just gid -> do
                  liftIO $ addAltAccount man gid uuid
                  psa <- liftIO $ atomically $ readTVar (bowBotAccounts bdt)
                  let oldAcc = head $ filter ((==gid) . accountId) psa
                  liftIO $ atomically $ writeTVar (bowBotAccounts bdt) (oldAcc { accountMinecrafts = uuid:accountMinecrafts oldAcc } : filter ((/= gid) . accountId) psa)
                  liftIO $ onComplete man uuid
                  updateDiscordRolesSingleId bdt man did
                  respond m "*Registered successfully*"
            else do
              pns <- fmap (>>=(\BowBotAccount {..} -> (, accountId) <$> accountDiscords)) $ liftIO $ atomically $ readTVar (bowBotAccounts bdt)
              case lookup did pns of
                Nothing -> do
                  newAcc <- liftIO $ addAccount man (head names) did uuid
                  case newAcc of
                    Nothing -> respond m somethingWrongMessage
                    Just newAcc' -> do
                      psa <- liftIO $ atomically $ readTVar (bowBotAccounts bdt)
                      liftIO $ atomically $ writeTVar (bowBotAccounts bdt) (newAcc':psa)
                      liftIO $ onComplete man uuid
                      updateDiscordRolesSingleId bdt man did
                      respond m "*Registered successfully*"
                Just _ -> respond m (if isself then "*You are already registered!*" else "*That person is already registered!*")