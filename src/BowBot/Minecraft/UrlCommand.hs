{-# LANGUAGE RecordWildCards #-}

module BowBot.Minecraft.UrlCommand where

import BowBot.Command
import BowBot.Minecraft.Basic
import BowBot.Minecraft.Account
import BowBot.Minecraft.Arg
import Discord.Types
import Control.Monad.Trans (lift)

urlCommand :: String -> String -> (UUID -> String) -> Command
urlCommand name desc url = Command CommandInfo
  { commandName = name
  , commandHelpEntries = [HelpEntry { helpUsage = name ++ " [name]", helpDescription = desc, helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument (\s -> lift (envs envSender) >>= flip minecraftArgFull s . userId) $ \MinecraftResponse {mcResponseAccount = MinecraftAccount {..}} -> do
    respond $ url mcUUID