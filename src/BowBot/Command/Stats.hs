module BowBot.Command.Stats where

import BowBot.Stats.HypixelBow
import BowBot.Command
import BowBot.Minecraft
import BowBot.Command.Register
import Data.Map ((!?))
import BowBot.Background

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
      return $ maybe (Left ()) Right st
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
hypixelBowStatsMessage settings (JustResponse n s) = "**" ++ n ++ ":**\n" ++ showHypixelBowStats settings s
hypixelBowStatsMessage settings (OldResponse o n s) = "**" ++ o ++ " (" ++ n ++ "):**\n" ++ showHypixelBowStats settings s
hypixelBowStatsMessage settings (DidYouMeanResponse n s) = "*Did you mean* **" ++ n ++ ":**\n" ++ showHypixelBowStats settings s
hypixelBowStatsMessage settings (DidYouMeanOldResponse o n s) = "*Did you mean* **" ++ o ++ " (" ++ n ++ "):**\n" ++ showHypixelBowStats settings s

hypixelBowTryRegister :: (APIMonad m, BotDataMonad m) => UUID -> [String] -> HypixelBowStats -> m ()
hypixelBowTryRegister uuid names s | bowWins s >= 50 = do
  registeredPlayers <- map mcUUID <$> hRead minecraftAccounts
  unless (uuid `elem` registeredPlayers) $ do
    acc <- addMinecraftAccount uuid names
    for_ acc $ \x -> hModify minecraftAccounts (x:)
  updateHypixelBowLeaderboard "none" (fromList [(uuid, hypixelBowStatsToLeaderboards s)])
hypixelBowTryRegister _ _ _ = pure ()