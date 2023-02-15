module BowBot.Minecraft.NameCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Minecraft.Arg
import Discord.Types
import Control.Monad.Trans (lift)
import qualified Data.Text as T

nameCommand :: Command
nameCommand = Command CommandInfo
  { commandName = "n"
  , commandHelpEntries = [HelpEntry { helpUsage = "n [name]", helpDescription = "show player's Minecraft name history", helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument (\s -> lift (envs envSender) >>= flip minecraftArgFull s . userId) $ \MinecraftResponse {mcResponseAccount = mcResponseAccount@MinecraftAccount {..}, ..} -> do
    let (didYouMean, renderedName) = (if mcResponseAutocorrect == ResponseAutocorrect then "*Did you mean*" else "Name history of", showMinecraftAccountDiscord mcResponseTime mcResponseAccount)
    respond $ didYouMean <> " " <> renderedName <> ":```\n" <> T.unlines mcNames <> "```"
