{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.Snipe.Command where

import BowBot.Command
import BowBot.Snipe.Basic
import BowBot.Discord.Account
import BowBot.BotData.Cached
import BowBot.Discord.Utils

snipeCommand :: Command
snipeCommand = Command CommandInfo
  { commandName = "snipe"
  , commandHelpEntries = [HelpEntry { helpUsage = "snipe", helpDescription = "show the last deleted message from this channel", helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 10
  } $ noArguments $ do
    channel <- envs envChannel
    msg <- getFromCache channel
    case msg of
      Nothing -> respond "*Nothing to snipe!*"
      Just SnipeMessage {..} -> do
        snipeAuthor' <- getFromCache snipeMessageAuthor
        case snipeAuthor' of
          Nothing -> respond somethingWentWrongMessage
          Just snipeAuthor -> respond $ showDiscordAccountDiscord snipeAuthor <> " *wrote " <> discordFormatTimestampFull snipeMessageTimestamp <> ":* \n" <> snipeMessageContent