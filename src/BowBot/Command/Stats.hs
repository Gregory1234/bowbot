module BowBot.Command.Stats where

import BowBot.Stats
import BowBot.Command
import BowBot.Minecraft
import Data.Proxy
import Discord
import Discord.Types
import Data.Text (unpack)
import Data.Char (isSpace)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)

data StatsCommandMode = AlwaysDefault | AlwaysAll | UserSettings

statsCommand :: StatType s => Proxy s -> String -> StatsCommandMode -> Command
statsCommand pr name mode = Command name 2 $ \m man bdt -> do
  res <- withMinecraft man bdt True (Left (dropWhile isSpace $ dropWhile (not . isSpace) $ unpack (messageText m))) $ \uuid _ -> do
    liftIO $ requestStats pr man uuid
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