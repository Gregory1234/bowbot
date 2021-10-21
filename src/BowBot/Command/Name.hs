module BowBot.Command.Name where

import BowBot.Command
import BowBot.Minecraft


nameCommand :: String -> Bool -> Command
nameCommand name ac = Command name DefaultLevel 2 $ \m man bdt -> do -- TODO: add times when changed and lengths
  let args = tail $ words (unpack $ messageText m)
  let player = case args of
        [] -> Right $ userId $ messageAuthor m
        _ -> Left $ unwords args
  names <- liftIO $ withMinecraft man bdt ac player $ \_ names -> return $ if null names then Left () else Right names
  respond m $ case names of
    PlayerNotFound -> playerNotFoundMessage
    DiscordUserNotFound -> discordNotFoundMessage
    (UserError ()) -> somethingWrongMessage
    NotOnList -> registerMessage
    (JustResponse n s) -> "Name history of **" ++ n ++ "**:```\n" ++ unlines s ++ "```"
    (OldResponse o n s) -> "Name history of **" ++ o ++ " (" ++ n ++ ")**:```\n" ++ unlines s ++ "```"
    (DidYouMeanResponse n s) -> "*Did you mean* **" ++ n ++ "**:```\n" ++ unlines s ++ "```"
    (DidYouMeanOldResponse o n s) -> "*Did you mean* **" ++ o ++ " (" ++ n ++ ")**:```\n" ++ unlines s ++ "```"