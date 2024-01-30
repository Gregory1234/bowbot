{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}

module BowBot.Discord.Orphans() where

import qualified Discord.Requests as R
import qualified Discord.Internal.Rest as R
import Discord.Types
import Control.DeepSeq (NFData(..), deepseq)
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Language.MySQL.Query (ToMysql, ToMysqlSimple, FromMysql, FromMysqlSimple, StateT(..))
import TextShow (TextShow(..))
import TextShow.Generic (FromGeneric(..))
import Discord (RestCallErrorCode(..))

deriving stock instance Generic Snowflake
deriving anyclass instance NFData Snowflake
deriving stock instance Generic (DiscordId a)
deriving anyclass instance NFData (DiscordId a)
deriving stock instance Generic Button
deriving anyclass instance NFData Button
deriving stock instance Generic ChannelTypeOption
deriving anyclass instance NFData ChannelTypeOption
deriving stock instance Generic SelectMenuData
deriving anyclass instance NFData SelectMenuData
deriving stock instance Generic SelectMenu
deriving anyclass instance NFData SelectMenu
deriving stock instance Generic ActionRow
deriving anyclass instance NFData ActionRow
deriving stock instance Generic RolePermissions
deriving anyclass instance NFData RolePermissions
deriving stock instance Generic (R.Base64Image a)
deriving anyclass instance NFData (R.Base64Image a)
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
deriving stock instance Generic R.StartThreadOpts
deriving anyclass instance NFData R.StartThreadOpts
deriving stock instance Generic R.StartThreadNoMessageOpts
deriving anyclass instance NFData R.StartThreadNoMessageOpts
deriving stock instance Generic R.DiscordColor
deriving anyclass instance NFData R.DiscordColor
deriving stock instance Generic R.ThreadMetadata
deriving anyclass instance NFData R.ThreadMetadata
deriving stock instance Generic R.ThreadMember
deriving anyclass instance NFData R.ThreadMember
deriving stock instance Generic R.GuildMember
deriving anyclass instance NFData R.GuildMember
deriving stock instance Generic R.ButtonStyle
deriving anyclass instance NFData R.ButtonStyle
deriving stock instance Generic R.Emoji
deriving anyclass instance NFData R.Emoji
deriving stock instance Generic R.SelectOption
deriving anyclass instance NFData R.SelectOption
deriving stock instance Generic R.GuildWidget
deriving anyclass instance NFData R.GuildWidget

instance NFData (R.ChannelRequest a) where
  rnf (R.GetChannel a) = a `deepseq` ()
  rnf (R.ModifyChannel a b) = a `deepseq` b `deepseq` ()
  rnf (R.DeleteChannel a) = a `deepseq` ()
  rnf (R.GetChannelMessages a b) = a `deepseq` b `deepseq` ()
  rnf (R.GetChannelMessage a) = a `deepseq` ()
  rnf (R.CreateMessage a b) = a `deepseq` b `deepseq` ()
  rnf (R.CreateMessageDetailed a b) = a `deepseq` b `deepseq` ()
  rnf (R.CreateReaction a b) = a `deepseq` b `deepseq` ()
  rnf (R.DeleteOwnReaction a b) = a `deepseq` b `deepseq` ()
  rnf (R.DeleteUserReaction a b c) = a `deepseq` b `deepseq` c `deepseq` ()
  rnf (R.DeleteSingleReaction a b) = a `deepseq` b `deepseq` ()
  rnf (R.GetReactions a b c) = a `deepseq` b `deepseq` c `deepseq` ()
  rnf (R.DeleteAllReactions a) = a `deepseq` ()
  rnf (R.EditMessage a b) = a `deepseq` b `deepseq` ()
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
  rnf (R.StartThreadFromMessage a b c) = a `deepseq` b `deepseq` c `deepseq` ()
  rnf (R.StartThreadNoMessage a b) = a `deepseq` b `deepseq` ()
  rnf (R.JoinThread a) = a `deepseq` ()
  rnf (R.AddThreadMember a b) = a `deepseq` b `deepseq` ()
  rnf (R.LeaveThread a) = a `deepseq` ()
  rnf (R.RemoveThreadMember a b) = a `deepseq` b `deepseq` ()
  rnf (R.GetThreadMember a b) = a `deepseq` b `deepseq` ()
  rnf (R.ListThreadMembers a) = a `deepseq` ()
  rnf (R.ListPublicArchivedThreads a b) = a `deepseq` b `deepseq` ()
  rnf (R.ListPrivateArchivedThreads a b) = a `deepseq` b `deepseq` ()
  rnf (R.ListJoinedPrivateArchivedThreads a b) = a `deepseq` b `deepseq` ()

instance NFData (R.UserRequest a) where
  rnf R.GetCurrentUser = ()
  rnf (R.GetUser a) = a `deepseq` ()
  rnf (R.ModifyCurrentUser a b) = a `deepseq` b `deepseq` ()
  rnf R.GetCurrentUserGuilds = ()
  rnf (R.LeaveGuild a) = a `deepseq` ()
  rnf R.GetUserDMs = ()
  rnf (R.CreateDM a) = a `deepseq` ()
  rnf R.GetUserConnections = ()

instance NFData (R.GuildRequest a) where
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
  rnf (R.GetGuildWidget a) = a `deepseq` ()
  rnf (R.ModifyGuildWidget a b) = a `deepseq` b `deepseq` ()
  rnf (R.GetGuildVanityURL a) = a `deepseq` ()

-- TODO: reduce the number of orphaned instances

deriving newtype instance Num Snowflake
deriving newtype instance Hashable Snowflake
deriving newtype instance TextShow Snowflake
deriving newtype instance ToMysqlSimple Snowflake
deriving newtype instance FromMysqlSimple Snowflake
deriving newtype instance ToMysql Snowflake
deriving newtype instance FromMysql Snowflake
deriving newtype instance Num (DiscordId a)
deriving newtype instance Hashable (DiscordId a)
deriving newtype instance TextShow (DiscordId a)
deriving newtype instance ToMysqlSimple (DiscordId a)
deriving newtype instance FromMysqlSimple (DiscordId a)
deriving newtype instance ToMysql (DiscordId a)
deriving newtype instance FromMysql (DiscordId a)

deriving stock instance Generic RestCallErrorCode
deriving via (FromGeneric RestCallErrorCode) instance TextShow RestCallErrorCode

