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
  args <- hArgs
  caller <- hCaller
  let player = case args of
        [] -> Right (userId caller)
        mcname -> Left (unwords mcname)
  tryApiRequests (rc bdt) 2 (\sec -> hRespond $ "**Too many requests! Wait another " ++ show sec ++ " seconds!**") $ do
    res <- withMinecraft bdt False player $ \uuid names -> do
      st <- requestStats pr uuid
      for_ st (tryRegister bdt uuid names)
      return $ maybe (Left ()) Right st
    settings <- case mode of
      AlwaysDefault -> pure defSettings
      AlwaysAll -> pure allSettings
      UserSettings -> do
        settings <- readProp discordSettings bdt
        pure $ fromMaybe defSettings $ settings !? userId caller
    hRespond $ statsMessage settings res
    hMDiscord $ updateDiscordRolesSingleId bdt (userId caller)

statsMessage :: StatType s => Settings -> MinecraftResponse () s -> String
statsMessage _ PlayerNotFound = playerNotFoundMessage
statsMessage _ DiscordUserNotFound = discordNotFoundMessage
statsMessage _ (UserError ()) = playerNotFoundMessage -- TODO: add a better message here
statsMessage _ NotOnList = registerMessage
statsMessage settings (JustResponse n s) = "**" ++ n ++ ":**\n" ++ showStats settings s
statsMessage settings (OldResponse o n s) = "**" ++ o ++ " (" ++ n ++ "):**\n" ++ showStats settings s
statsMessage settings (DidYouMeanResponse n s) = "*Did you mean* **" ++ n ++ ":**\n" ++ showStats settings s
statsMessage settings (DidYouMeanOldResponse o n s) = "*Did you mean* **" ++ o ++ " (" ++ n ++ "):**\n" ++ showStats settings s

tryRegister :: (StatType s, APIMonad m) => BotData -> String -> [String] -> s -> m ()
tryRegister bdt uuid names s | statsNotable s = do
  registeredPlayers <- map mcUUID <$> readProp minecraftAccounts bdt
  unless (uuid `elem` registeredPlayers) $ do
    acc <- addMinecraftAccount uuid names
    for_ acc $ \x -> modifyProp minecraftAccounts bdt (x:)
  updateLeaderboard (fromList [(uuid, toLeaderboard s)])
tryRegister _ _ _ _ = pure ()