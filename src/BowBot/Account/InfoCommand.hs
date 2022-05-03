{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module BowBot.Account.InfoCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Account.Arg
import Discord.Types
import Control.Monad.Trans (lift)
import BowBot.Minecraft.Arg
import BowBot.Discord.Account
import BowBot.Account.Basic
import Data.Proxy
import BowBot.BotData.Cached (getCacheMap)
import qualified Data.HashMap.Strict as HM
import BowBot.Minecraft.Basic (uuidString)

infoCommand :: Command
infoCommand = Command CommandInfo
  { commandName = "i"
  , commandHelpEntries = [HelpEntry { helpUsage = "i [name]", helpDescription = "show info about a player", helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ hOneOptionalArgument (\s -> lift (hEnv envSender) >>= accountArgDefault s . userId) $ \AccountResponse { accResponseAccount = BowBotAccount {..}, ..} -> do
    let (didYouMean, renderedName) = case accResponseCause of
          (Left acc) -> ("", showDiscordAccount acc)
          (Right (typ, MinecraftAccount {..})) -> case typ of
              JustResponse -> ("", head mcNames)
              OldResponse o -> ("", o ++ " (" ++ head mcNames ++ ")")
              DidYouMeanResponse -> ("*Did you mean* ", head mcNames)
              DidYouMeanOldResponse o -> ("*Did you mean* ", o ++ " (" ++ head mcNames ++ ")")
    mc <- getCacheMap (Proxy @MinecraftAccount)
    dc <- getCacheMap (Proxy @DiscordAccount)
    let mcAccs = unlines $ map (\uuid -> let MinecraftAccount {..} = mc HM.! uuid in (if mcUUID == accountSelectedMinecraft then "*" else " ") ++ head mcNames ++ " (" ++ uuidString mcUUID ++ ")") accountMinecrafts
    let dcAccs = unlines $ map (\did -> let acc@DiscordAccount {..} = dc HM.! did in showDiscordAccount acc ++ ", id " ++ show discordId ) accountDiscords
    hRespond $ didYouMean ++ "**" ++ renderedName ++ ":**\n" ++ " - Bow Bot id: " ++ show accountId ++ "\n" ++ " - Minecraft accounts:```\n" ++ mcAccs ++ "```" ++ " - Discord accounts: ```\n" ++ dcAccs ++ "```"
