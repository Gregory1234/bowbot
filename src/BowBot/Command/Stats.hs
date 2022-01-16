module BowBot.Command.Stats where

import BowBot.Stats.HypixelBow
import BowBot.Command
import BowBot.Minecraft
import BowBot.Command.Register
import Data.Map ((!?))
import BowBot.Background
import Data.List (intercalate)
import Data.Maybe (catMaybes)

data StatsCommandMode = AlwaysDefault | AlwaysAll | UserSettings

hypixelBowStatsCommand :: String -> StatsCommandMode -> Command
hypixelBowStatsCommand name mode = Command name DefaultLevel 15 $ do
  args <- hArgs
  caller <- hCaller
  let player = case args of
        [] -> Right (userId caller)
        mcname -> Left (unwords mcname)
  hTryApiRequests hypixelRequestCounter 2 (\sec -> hRespond $ "**Too many requests! Wait another " ++ show sec ++ " seconds!**") $ do
    res <- withMinecraft False player $ \uuid names -> do
      st <- requestHypixelBowStats uuid
      for_ st (hypixelBowTryRegister uuid names)
      return $ maybe (Left ()) (\s -> if bowWins s + bowLosses s > 0 then Right s else Left ()) st
    settings <- case mode of
      AlwaysDefault -> pure defSettings
      AlwaysAll -> pure allSettings
      UserSettings -> do
        settings <- hRead discordSettings
        pure $ fromMaybe defSettings $ settings !? userId caller
    hRespond $ hypixelBowStatsMessage settings res
    updateDiscordRolesSingleId (userId caller)

hypixelBowStatsMessage :: Settings -> MinecraftResponse () HypixelBowStats -> String
hypixelBowStatsMessage _ PlayerNotFound = playerNotFoundMessage
hypixelBowStatsMessage _ DiscordUserNotFound = discordNotFoundMessage
hypixelBowStatsMessage _ (UserError ()) = playerNotFoundMessage -- TODO: add a better message here
hypixelBowStatsMessage _ NotOnList = registerMessage
hypixelBowStatsMessage settings (JustResponse n s) = "**" ++ discordEscape n ++ ":**\n" ++ showHypixelBowStats settings s
hypixelBowStatsMessage settings (OldResponse o n s) = "**" ++ discordEscape o ++ " (" ++ discordEscape n ++ "):**\n" ++ showHypixelBowStats settings s
hypixelBowStatsMessage settings (DidYouMeanResponse n s) = "*Did you mean* **" ++ discordEscape n ++ ":**\n" ++ showHypixelBowStats settings s
hypixelBowStatsMessage settings (DidYouMeanOldResponse o n s) = "*Did you mean* **" ++ discordEscape o ++ " (" ++ discordEscape n ++ "):**\n" ++ showHypixelBowStats settings s

hypixelBowTimeStatsCommand :: String -> [TimeStatsType] -> StatsCommandMode -> Command
hypixelBowTimeStatsCommand name times mode = Command name DefaultLevel 15 $ do
  args <- hArgs
  caller <- hCaller
  let player = case args of
        [] -> Right (userId caller)
        mcname -> Left (unwords mcname)
  hTryApiRequests hypixelRequestCounter 2 (\sec -> hRespond $ "**Too many requests! Wait another " ++ show sec ++ " seconds!**") $ do
    res <- withMinecraft False player $ \uuid names -> do
      st <- requestHypixelBowStats uuid
      for_ st (hypixelBowTryRegister uuid names)
      st' <- for st $ \s -> catMaybes <$> for times (requestHypixelBowTimeStats s uuid)
      return $ maybe (Left ()) Right $ st' >>= \s -> if null s then Nothing else Just s
    settings <- case mode of
      AlwaysDefault -> pure defSettings
      AlwaysAll -> pure allSettings
      UserSettings -> do
        settings <- hRead discordSettings
        pure $ fromMaybe defSettings $ settings !? userId caller
    hRespond $ hypixelBowTimeStatsMessage settings res

hypixelBowTimeStatsMessage :: Settings -> MinecraftResponse () [HypixelBowTimeStats] -> String
hypixelBowTimeStatsMessage _ PlayerNotFound = playerNotFoundMessage
hypixelBowTimeStatsMessage _ DiscordUserNotFound = discordNotFoundMessage
hypixelBowTimeStatsMessage _ (UserError ()) = playerNotFoundMessage -- TODO: add a better message here
hypixelBowTimeStatsMessage _ NotOnList = registerMessage
hypixelBowTimeStatsMessage settings (JustResponse n s) = "**" ++ discordEscape n ++ ":**\n" ++ intercalate "\n" (map (showHypixelBowTimeStats settings) s)
hypixelBowTimeStatsMessage settings (OldResponse o n s) = "**" ++ discordEscape o ++ " (" ++ discordEscape n ++ "):**\n" ++ intercalate "\n" (map (showHypixelBowTimeStats settings) s)
hypixelBowTimeStatsMessage settings (DidYouMeanResponse n s) = "*Did you mean* **" ++ discordEscape n ++ ":**\n" ++ intercalate "\n" (map (showHypixelBowTimeStats settings) s)
hypixelBowTimeStatsMessage settings (DidYouMeanOldResponse o n s) = "*Did you mean* **" ++ discordEscape o ++ " (" ++ discordEscape n ++ "):**\n" ++ intercalate "\n" (map (showHypixelBowTimeStats settings) s)

hypixelBowTryRegister :: (APIMonad m, DBMonad m, BotDataMonad m, MonadHoistIO m) => UUID -> [String] -> HypixelBowStats -> m ()
hypixelBowTryRegister uuid names s | bowWins s >= 50 = do
  registeredPlayers <- map mcUUID <$> hRead minecraftAccounts
  unless (uuid `elem` registeredPlayers) $ do
    acc <- addMinecraftAccount uuid names
    for_ acc $ \x -> hModify minecraftAccounts (x:)
  updateHypixelBowLeaderboard [] (fromList [(uuid, hypixelBowStatsToLeaderboards s)])
hypixelBowTryRegister _ _ _ = pure ()