{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.Account.InfoCommand where

import BowBot.Command
import BowBot.Minecraft.Account
import BowBot.Account.Arg
import Discord.Types
import BowBot.Minecraft.Arg
import BowBot.Discord.Account
import BowBot.Account.Basic
import BowBot.BotData.Cached (getCacheMap)
import qualified Data.HashMap.Strict as HM
import BowBot.Minecraft.Basic (uuidString)
import qualified Data.Text as T
import BowBot.Utils

infoCommand :: Command
infoCommand = Command CommandInfo
  { commandName = "i"
  , commandHelpEntries = [HelpEntry { helpUsage = "i [name]", helpDescription = "show info about a player", helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument (\s -> lift (envs envSender) >>= flip accountArgFull s . userId) $ \AccountResponse { accResponseAccount = BowBotAccount {..}, ..} -> do
    let (didYouMean, renderedName) = case accResponseType of
          (AccountDiscordResponse acc) -> ("", showDiscordAccountDiscord acc)
          (AccountMinecraftResponse MinecraftResponse {..}) -> (if mcResponseAutocorrect == ResponseAutocorrect then "*Did you mean* " else "", showMinecraftAccountDiscord mcResponseTime mcResponseAccount)
    mc <- getCacheMap
    dc <- getCacheMap
    let mcAccs = T.unlines $ map (\uuid -> let MinecraftAccount {..} = mc HM.! uuid in (if mcUUID == accountSelectedMinecraft then "*" else " ") <> head mcNames <> " (" <> uuidString mcUUID <> ")") accountMinecrafts
    let dcAccs = T.unlines $ map (\did -> let acc@DiscordAccount {..} = dc HM.! did in (if discordIsMember then "*" else " ") <> showDiscordAccount acc <> ", id " <> showt discordId ) accountDiscords
    respond $ didYouMean <> renderedName <> ":\n" <> " - Bow Bot id: " <> showt accountId <> "\n" <> " - Minecraft accounts:```\n" <> mcAccs <> "```" <> " - Discord accounts: ```\n" <> dcAccs <> "```"
