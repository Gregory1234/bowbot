{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module BowBot.DiscordNFData() where

import qualified Discord.Requests as R
import qualified Discord.Internal.Rest as R
import Discord.Types
import Control.DeepSeq (NFData(..), deepseq)
import GHC.Generics (Generic)

deriving stock instance Generic Snowflake
deriving anyclass instance NFData Snowflake
deriving stock instance Generic R.Overwrite
deriving anyclass instance NFData R.Overwrite
deriving stock instance Generic R.ModifyChannelOpts
deriving anyclass instance NFData R.ModifyChannelOpts
deriving stock instance Generic R.MessageTiming
deriving anyclass instance NFData R.MessageTiming
deriving stock instance Generic R.CreateEmbedImage
deriving anyclass instance NFData R.CreateEmbedImage
deriving stock instance Generic R.EmbedField
deriving anyclass instance NFData R.EmbedField
deriving stock instance Generic R.CreateEmbed
deriving anyclass instance NFData R.CreateEmbed
deriving stock instance Generic R.AllowedMentions
deriving anyclass instance NFData R.AllowedMentions
deriving stock instance Generic R.MessageReference
deriving anyclass instance NFData R.MessageReference
deriving stock instance Generic R.MessageDetailedOpts
deriving anyclass instance NFData R.MessageDetailedOpts
deriving stock instance Generic R.ReactionTiming
deriving anyclass instance NFData R.ReactionTiming
deriving stock instance Generic R.ChannelPermissionsOptsType
deriving anyclass instance NFData R.ChannelPermissionsOptsType
deriving stock instance Generic R.ChannelPermissionsOpts
deriving anyclass instance NFData R.ChannelPermissionsOpts
deriving stock instance Generic R.ChannelInviteOpts
deriving anyclass instance NFData R.ChannelInviteOpts
deriving stock instance Generic R.GroupDMAddRecipientOpts
deriving anyclass instance NFData R.GroupDMAddRecipientOpts
deriving stock instance Generic R.User
deriving anyclass instance NFData R.User
deriving stock instance Generic R.Channel
deriving anyclass instance NFData R.Channel
deriving stock instance Generic R.CreateGuildOpts
deriving anyclass instance NFData R.CreateGuildOpts
deriving stock instance Generic R.ModifyGuildOpts
deriving anyclass instance NFData R.ModifyGuildOpts
deriving stock instance Generic R.CreateGuildChannelOpts
deriving anyclass instance NFData R.CreateGuildChannelOpts
deriving stock instance Generic R.GuildMembersTiming
deriving anyclass instance NFData R.GuildMembersTiming
deriving stock instance Generic R.AddGuildMemberOpts
deriving anyclass instance NFData R.AddGuildMemberOpts
deriving stock instance Generic R.ModifyGuildMemberOpts
deriving anyclass instance NFData R.ModifyGuildMemberOpts
deriving stock instance Generic R.CreateGuildBanOpts
deriving anyclass instance NFData R.CreateGuildBanOpts
deriving stock instance Generic R.ModifyGuildRoleOpts
deriving anyclass instance NFData R.ModifyGuildRoleOpts
deriving stock instance Generic R.CreateGuildIntegrationOpts
deriving anyclass instance NFData R.CreateGuildIntegrationOpts
deriving stock instance Generic R.ModifyGuildIntegrationOpts
deriving anyclass instance NFData R.ModifyGuildIntegrationOpts
deriving stock instance Generic R.GuildEmbed
deriving anyclass instance NFData R.GuildEmbed

instance NFData (R.ChannelRequest a) where
  rnf (R.GetChannelMessage a) = a `deepseq` ()
  rnf (R.GetChannel a) = a `deepseq` ()
  rnf (R.ModifyChannel a b) = a `deepseq` b `deepseq` ()
  rnf (R.DeleteChannel a) = a `deepseq` ()
  rnf (R.GetChannelMessages a b) = a `deepseq` b `deepseq` ()
  rnf (R.CreateMessage a b) = a `deepseq` b `deepseq` ()
  rnf (R.CreateMessageEmbed a b c) = a `deepseq` b `deepseq` c `deepseq` ()
  rnf (R.CreateMessageUploadFile a b c) = a `deepseq` b `deepseq` c `deepseq` ()
  rnf (R.CreateMessageDetailed a b) = a `deepseq` b `deepseq` ()
  rnf (R.CreateReaction a b) = a `deepseq` b `deepseq` ()
  rnf (R.DeleteOwnReaction a b) = a `deepseq` b `deepseq` ()
  rnf (R.DeleteUserReaction a b c) = a `deepseq` b `deepseq` c `deepseq` ()
  rnf (R.DeleteSingleReaction a b) = a `deepseq` b `deepseq` ()
  rnf (R.GetReactions a b c) = a `deepseq` b `deepseq` c `deepseq` ()
  rnf (R.DeleteAllReactions a) = a `deepseq` ()
  rnf (R.EditMessage a b c) = a `deepseq` b `deepseq` c `deepseq` ()
  rnf (R.DeleteMessage a) = a `deepseq` ()
  rnf (R.BulkDeleteMessage a) = a `deepseq` ()
  rnf (R.EditChannelPermissions a b c) = a `deepseq` b `deepseq` c `deepseq` ()
  rnf (R.GetChannelInvites a) = a `deepseq` ()
  rnf (R.CreateChannelInvite a b) = a `deepseq` b `deepseq` ()
  rnf (R.DeleteChannelPermission a b) = a `deepseq` b `deepseq` ()
  rnf (R.TriggerTypingIndicator a) = a `deepseq` ()
  rnf (R.GetPinnedMessages a) = a `deepseq` ()
  rnf (R.AddPinnedMessage a) = a `deepseq` ()
  rnf (R.DeletePinnedMessage a) = a `deepseq` ()
  rnf (R.GroupDMAddRecipient a b) = a `deepseq` b `deepseq` ()
  rnf (R.GroupDMRemoveRecipient a b) = a `deepseq` b `deepseq` ()

instance NFData (R.UserRequest a) where
  rnf R.GetCurrentUser = ()
  rnf (R.GetUser a) = a `deepseq` ()
  rnf (R.ModifyCurrentUser a b) = a `deepseq` b `seq` () -- TODO: deepseq impossible
  rnf R.GetCurrentUserGuilds = ()
  rnf (R.LeaveGuild a) = a `deepseq` ()
  rnf R.GetUserDMs = ()
  rnf (R.CreateDM a) = a `deepseq` ()
  rnf R.GetUserConnections = ()

instance NFData (R.GuildRequest a) where
  rnf (R.CreateGuild a) = a `deepseq` ()
  rnf (R.GetGuild a) = a `deepseq` ()
  rnf (R.ModifyGuild a b) = a `deepseq` b `deepseq` ()
  rnf (R.DeleteGuild a) = a `deepseq` ()
  rnf (R.GetGuildChannels a) = a `deepseq` ()
  rnf (R.CreateGuildChannel a b c d) = a `deepseq` b `deepseq` c `deepseq` d `deepseq` ()
  rnf (R.ModifyGuildChannelPositions a b) = a `deepseq` b `deepseq` ()
  rnf (R.GetGuildMember a b) = a `deepseq` b `deepseq` ()
  rnf (R.ListGuildMembers a b) = a `deepseq` b `deepseq` ()
  rnf (R.AddGuildMember a b c) = a `deepseq` b `deepseq` c `deepseq` ()
  rnf (R.ModifyGuildMember a b c) = a `deepseq` b `deepseq` c `deepseq` ()
  rnf (R.ModifyCurrentUserNick a b) = a `deepseq` b `deepseq` ()
  rnf (R.AddGuildMemberRole a b c) = a `deepseq` b `deepseq` c `deepseq` ()
  rnf (R.RemoveGuildMemberRole a b c) = a `deepseq` b `deepseq` c `deepseq` ()
  rnf (R.RemoveGuildMember a b) = a `deepseq` b `deepseq` ()
  rnf (R.GetGuildBans a) = a `deepseq` ()
  rnf (R.GetGuildBan a b) = a `deepseq` b `deepseq` ()
  rnf (R.CreateGuildBan a b c) = a `deepseq` b `deepseq` c `deepseq` ()
  rnf (R.RemoveGuildBan a b) = a `deepseq` b `deepseq` ()
  rnf (R.GetGuildRoles a) = a `deepseq` ()
  rnf (R.CreateGuildRole a b) = a `deepseq` b `deepseq` ()
  rnf (R.ModifyGuildRolePositions a b) = a `deepseq` b `deepseq` ()
  rnf (R.ModifyGuildRole a b c) = a `deepseq` b `deepseq` c `deepseq` ()
  rnf (R.DeleteGuildRole a b) = a `deepseq` b `deepseq` ()
  rnf (R.GetGuildPruneCount a b) = a `deepseq` b `deepseq` ()
  rnf (R.BeginGuildPrune a b) = a `deepseq` b `deepseq` ()
  rnf (R.GetGuildVoiceRegions a) = a `deepseq` ()
  rnf (R.GetGuildInvites a) = a `deepseq` ()
  rnf (R.GetGuildIntegrations a) = a `deepseq` ()
  rnf (R.CreateGuildIntegration a b c) = a `deepseq` b `deepseq` c `deepseq` ()
  rnf (R.ModifyGuildIntegration a b c) = a `deepseq` b `deepseq` c `deepseq` ()
  rnf (R.DeleteGuildIntegration a b) = a `deepseq` b `deepseq` ()
  rnf (R.SyncGuildIntegration a b) = a `deepseq` b `deepseq` ()
  rnf (R.GetGuildEmbed a) = a `deepseq` ()
  rnf (R.ModifyGuildEmbed a b) = a `deepseq` b `deepseq` ()
  rnf (R.GetGuildVanityURL a) = a `deepseq` ()