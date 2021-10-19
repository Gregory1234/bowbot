module BowBot.Command.Simple where

import BowBot.Command
import BowBot.Minecraft
import BowBot.BotData
import Control.Monad.IO.Class (liftIO)
import Discord.Types
import Data.Text (unpack)
import Data.Char (isSpace)
import Control.Monad (void)
  
urlCommand :: String -> Bool -> (String -> String) -> Command
urlCommand name ac mkurl = Command name DefaultLevel 2 $ \m man bdt -> do
  let args = words $ dropWhile isSpace $ dropWhile (not . isSpace) $ unpack (messageText m)
  let player = case args of
        [] -> Right (userId $ messageAuthor m)
        mcname -> Left (unwords mcname)
  url <- liftIO $ withMinecraft man bdt ac player $ \u _ -> do
    return (Just $ mkurl u)
  void $ case url of
    NoResponse -> respond m "*The player doesn't exist!*"
    (JustResponse _ url') -> respond m url'
    (OldResponse _ _ url') -> respond m url'
    (DidYouMeanResponse n url') -> do
      respond m $ "*Did you mean* **" ++ n ++ "**:"
      respond m url'
    (DidYouMeanOldResponse n o url') -> do
      respond m $ "*Did you mean* **" ++ o ++ " (" ++ n ++ ")**:"
      respond m url'
    NotOnList -> sendRegisterMessage m

constStringCommand :: String -> PermissionLevel -> String -> Command
constStringCommand name lvl str = Command name lvl 2 $ \m _ _ -> respond m str