{-# LANGUAGE RecordWildCards #-}

module BowBot.Snipe.Command where

import BowBot.Command
import BowBot.Snipe.Basic
import BowBot.Discord.Account
import BowBot.BotData.Cached

snipeCommand :: Command
snipeCommand = Command CommandInfo
  { commandName = "snipe"
  , commandHelpEntries = [HelpEntry { helpUsage = "snipe", helpDescription = "show the last deleted message from this channel", helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 10
  } $ hNoArguments $ do
    channel <- hEnv envChannel
    msg <- getFromCache channel
    case msg of
      Nothing -> hRespond "*Nothing to snipe!*"
      Just SnipeMessage {..} -> do
        snipeAuthor' <- getFromCache snipeMessageAuthor
        case snipeAuthor' of
          Nothing -> hRespond somethingWentWrongMessage
          Just snipeAuthor -> hRespond $ showDiscordAccountDiscord snipeAuthor ++ " *wrote:* \n" ++ snipeMessageContent