module BowBot.Snipe.Command where

import BowBot.Command
import BowBot.Snipe.Basic
import BowBot.Discord.Account
import BowBot.BotData.Cached
import BowBot.Discord.Utils

snipeCommand :: Command
snipeCommand = Command CommandInfo
  { commandName = "snipe"
  , commandHelpEntries = [HelpEntry { helpUsage = "snipe", helpDescription = "show the last deleted or edited message from this channel", helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 10
  } $ noArguments $ do
    channel <- envs envChannel
    msg <- getSnipeMessageByChannel channel
    case msg of
      Nothing -> respond "*Nothing to snipe!*"
      Just SnipeMessage {..} -> do
        snipeAuthor' <- getFromCache snipeMessageAuthor
        case snipeAuthor' of
          Nothing -> respond somethingWentWrongMessage
          Just snipeAuthor -> respond $ if snipeMessageWasEdited
              then showDiscordAccountDiscord snipeAuthor <> " *edited " <> discordFormatTimestampFull snipeMessageTimestamp <> ":* \n" <> snipeMessageContent
              else showDiscordAccountDiscord snipeAuthor <> " *deleted " <> discordFormatTimestampFull snipeMessageTimestamp <> ":* \n" <> snipeMessageContent