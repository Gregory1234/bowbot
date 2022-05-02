{-# LANGUAGE RecordWildCards #-}

module BowBot.Minecraft.NameCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Minecraft.Arg
import Discord.Types
import Control.Monad.Trans (lift)

nameCommand :: Command
nameCommand = Command CommandInfo
  { commandName = "n"
  , commandUsage = "n [name]"
  , commandDescription = "show player's Minecraft name history"
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  , commandGroup = "normal"
  } $ hOneOptionalArgument (\s -> lift (hEnv envSender) >>= minecraftArgDefault (const $ return (True, ())) s . userId) $ \MinecraftResponse {responseAccount = MinecraftAccount {..}, ..} -> do
    let (didYouMean, renderedName) = case responseType of
          JustResponse -> ("Name history of", head mcNames)
          OldResponse o -> ("Name history of", o ++ " (" ++ head mcNames ++ ")")
          DidYouMeanResponse -> ("*Did you mean*", head mcNames)
          DidYouMeanOldResponse o -> ("*Did you mean*", o ++ " (" ++ head mcNames ++ ")")
    hRespond $ didYouMean ++ " **" ++ renderedName ++ "**:```\n" ++ unlines mcNames ++ "```"
