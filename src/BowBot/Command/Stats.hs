module BowBot.Command.Stats where

import BowBot.Stats
import BowBot.Command
import BowBot.Minecraft
import BowBot.Command.Register
import Data.Map ((!?))
import BowBot.Background

data StatsCommandMode = AlwaysDefault | AlwaysAll | UserSettings

statsCommand :: StatType s => Proxy s -> String -> (BotData -> ApiRequestCounter) -> StatsCommandMode -> Command
statsCommand pr name rc mode = Command name DefaultLevel 15 $ do
  bdt <- hData
  man <- hManager
  args <- hArgs
  caller <- hCaller
  let player = case args of
        [] -> Right (userId caller)
        mcname -> Left (unwords mcname)
  tryApiRequests (rc bdt) 2 (\sec -> hRespond $ "**Too many requests! Wait another " ++ show sec ++ " seconds!**") $ do
    res <- withMinecraft man bdt False player $ \uuid names -> do
      st <- liftIO $ requestStats pr man uuid
      for_ st (liftIO . tryRegister bdt man uuid names)
      return $ maybe (Left ()) Right st
    settings <- case mode of
      AlwaysDefault -> pure defSettings
      AlwaysAll -> pure allSettings
      UserSettings -> do
        settings <- readProp discordSettings bdt
        pure $ fromMaybe defSettings $ settings !? userId caller
    hRespond $ statsMessage settings res
    hDiscord $ updateDiscordRolesSingleId bdt man (userId caller)

statsMessage :: StatType s => Settings -> MinecraftResponse () s -> String
statsMessage _ PlayerNotFound = playerNotFoundMessage
statsMessage _ DiscordUserNotFound = discordNotFoundMessage
statsMessage _ (UserError ()) = playerNotFoundMessage -- TODO: add a better message here
statsMessage _ NotOnList = registerMessage
statsMessage settings (JustResponse n s) = "**" ++ n ++ ":**\n" ++ showStats settings s
statsMessage settings (OldResponse o n s) = "**" ++ o ++ " (" ++ n ++ "):**\n" ++ showStats settings s
statsMessage settings (DidYouMeanResponse n s) = "*Did you mean* **" ++ n ++ ":**\n" ++ showStats settings s
statsMessage settings (DidYouMeanOldResponse o n s) = "*Did you mean* **" ++ o ++ " (" ++ n ++ "):**\n" ++ showStats settings s

tryRegister :: StatType s => BotData -> Manager -> String -> [String] -> s -> IO ()
tryRegister bdt manager uuid names s | statsNotable s = do
  registeredPlayers <- atomically $ map mcUUID <$> readTVar (minecraftAccounts bdt)
  unless (uuid `elem` registeredPlayers) $ do
    acc <- addMinecraftAccount manager uuid names
    atomically $ for_ acc $ \x -> modifyTVar (minecraftAccounts bdt) (x:)
  updateLeaderboard manager (fromList [(uuid, toLeaderboard s)])
tryRegister _ _ _ _ _ = pure ()