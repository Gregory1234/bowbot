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
  , commandHelpEntries = [HelpEntry { helpUsage = "n [name]", helpDescription = "show player's Minecraft name history", helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ hOneOptionalArgument (\s -> lift (hEnv envSender) >>= minecraftArgDefault (const $ return (True, ())) s . userId) $ \MinecraftResponse {responseAccount = responseAccount@MinecraftAccount {..}, ..} -> do
    let (didYouMean, renderedName) = (if isDidYouMean responseType then "*Did you mean*" else "Name history of", showMinecraftAccountDiscord responseType responseAccount)
    hRespond $ didYouMean ++ " **" ++ renderedName ++ "**:```\n" ++ unlines mcNames ++ "```"
