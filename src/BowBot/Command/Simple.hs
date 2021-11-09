module BowBot.Command.Simple where

import BowBot.Command
import BowBot.Minecraft
import Data.Char (isSpace)
import Data.Void (absurd)

urlCommand :: String -> Bool -> (String -> String) -> Command
urlCommand name ac mkurl = Command name DefaultLevel 8 $ \m man bdt -> do
  let args = words $ dropWhile isSpace $ dropWhile (not . isSpace) $ unpack (messageText m)
  let player = case args of
        [] -> Right (userId $ messageAuthor m)
        mcname -> Left (unwords mcname)
  url <- liftIO $ withMinecraft man bdt ac player $ \u _ -> do
    return (Right $ mkurl u)
  void $ case url of
    PlayerNotFound -> respond m playerNotFoundMessage
    DiscordUserNotFound -> respond m discordNotFoundMessage
    (UserError v) -> absurd v
    NotOnList -> respond m registerMessage
    (JustResponse _ url') -> respond m url'
    (OldResponse _ _ url') -> respond m url'
    (DidYouMeanResponse n url') -> do
      respond m $ "*Did you mean* **" ++ n ++ "**:"
      respond m url'
    (DidYouMeanOldResponse n o url') -> do
      respond m $ "*Did you mean* **" ++ o ++ " (" ++ n ++ ")**:"
      respond m url'

constStringCommand :: String -> PermissionLevel -> String -> Command
constStringCommand name lvl str = Command name lvl 2 $ \m _ _ -> respond m str