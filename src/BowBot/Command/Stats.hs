module BowBot.Command.Stats where

import BowBot.Stats
import BowBot.Command
import BowBot.Minecraft
import BowBot.BotData
import Data.Proxy
import Discord.Types
import Data.Text (unpack)
import Data.Char (isSpace)
import Control.Monad.IO.Class (liftIO)
import BowBot.Command.Register
import Network.HTTP.Conduit (Manager)
import Control.Concurrent.STM.TVar (modifyTVar, readTVar)
import Control.Concurrent.STM (atomically)
import Data.Foldable (for_)
import Control.Monad (unless)
import Data.Map (fromList)

data StatsCommandMode = AlwaysDefault | AlwaysAll | UserSettings

statsCommand :: StatType s => Proxy s -> String -> (BotData -> ApiRequestCounter) -> StatsCommandMode -> Command
statsCommand pr name rc mode = Command name 2 $ \m man bdt -> do
  let args = words $ dropWhile isSpace $ dropWhile (not . isSpace) $ unpack (messageText m)
  let player = case args of
        [] -> Right (userId $ messageAuthor m)
        mcname -> Left (unwords mcname)
  tryApiRequests (rc bdt) 2 (\sec -> respond m $ "**Too many requests! Wait another " ++ show sec ++ " seconds!**") $ do
    res <- withMinecraft man bdt False player $ \uuid names -> do
      st <- liftIO $ requestStats pr man uuid
      for_ st (liftIO . tryRegister bdt man uuid names)
      return st
    settings <- case mode of
      AlwaysDefault -> pure $ defSettings pr
      AlwaysAll -> pure $ allSettings pr
      UserSettings -> do
        pure $ defSettings pr
    respond m $ statsMessage settings res

statsMessage :: StatType s => Settings s -> MinecraftResponse s -> String
statsMessage _ NoResponse = "*The player doesn't exist!*"
statsMessage settings (JustResponse n s) = "**" ++ n ++ ":**\n" ++ showStats settings s
statsMessage settings (OldResponse o n s) = "**" ++ o ++ " (" ++ n ++ "):**\n" ++ showStats settings s
statsMessage settings (DidYouMeanResponse n s) = "*Did you mean* **" ++ n ++ ":**\n" ++ showStats settings s
statsMessage settings (DidYouMeanOldResponse o n s) = "*Did you mean* **" ++ o ++ " (" ++ n ++ "):**\n" ++ showStats settings s
statsMessage _ NotOnList = registerMessage

tryRegister :: StatType s => BotData -> Manager -> String -> [String] -> s -> IO ()
tryRegister bdt manager uuid names s | statsNotable s = do
  registeredPlayers <- liftIO $ atomically $ map mcUUID <$> readTVar (minecraftAccounts bdt)
  unless (uuid `elem` registeredPlayers) $ do
    acc <- addMinecraftAccount manager uuid names
    for_ acc $ \x -> atomically $ modifyTVar (minecraftAccounts bdt) (x:)
  updateLeaderboard manager (fromList [(uuid, toLeaderboard s)])
tryRegister _ _ _ _ _ = pure ()