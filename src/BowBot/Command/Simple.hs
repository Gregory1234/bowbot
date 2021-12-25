module BowBot.Command.Simple where

import BowBot.Command
import BowBot.Minecraft
import Data.Void (absurd)

urlCommand :: String -> Bool -> (String -> String) -> Command
urlCommand name ac mkurl = Command name DefaultLevel 8 $ do
  man <- hManager
  bdt <- hData
  pName <- hArg 1
  caller <- hCaller
  let player = case pName of
        Nothing -> Right (userId caller)
        Just mcname -> Left mcname
  url <- liftIO $ withMinecraft man bdt ac player $ \u _ -> do
    return (Right $ mkurl u)
  void $ case url of
    PlayerNotFound -> hRespond playerNotFoundMessage
    DiscordUserNotFound -> hRespond discordNotFoundMessage
    (UserError v) -> absurd v
    NotOnList -> hRespond registerMessage
    (JustResponse _ url') -> hRespond url'
    (OldResponse _ _ url') -> hRespond url'
    (DidYouMeanResponse n url') -> do
      hRespond $ "*Did you mean* **" ++ n ++ "**:"
      hRespond url'
    (DidYouMeanOldResponse n o url') -> do
      hRespond $ "*Did you mean* **" ++ o ++ " (" ++ n ++ ")**:"
      hRespond url'

helpCommand :: String -> PermissionLevel -> (String -> String) -> Command
helpCommand name lvl str = Command name lvl 2 $ do
  prefix <- hRead discordCommandPrefix
  hRespond (str prefix)

constStringCommand :: String -> PermissionLevel -> String -> Command
constStringCommand name lvl str = Command name lvl 2 $ hRespond str