{-# LANGUAGE TypeFamilies #-}

module BowBot.Discord.Table where

import BowBot.Discord.Utils
import BowBot.DB.Typed

data DiscordTable a where
  DiscordTId :: DiscordTable UserId
  DiscordTName :: DiscordTable Text
  DiscordTDiscrim :: DiscordTable (Maybe Text)
  DiscordTNickname :: DiscordTable (Maybe Text)
  DiscordTIsMember :: DiscordTable Bool

instance DatabaseTableLike DiscordTable where
  columnName DiscordTId = "id"
  columnName DiscordTName = "name"
  columnName DiscordTDiscrim = "discriminator"
  columnName DiscordTNickname = "nickname"
  columnName DiscordTIsMember = "member"

instance DatabaseTable DiscordTable where
  newtype PrimaryKey DiscordTable = DiscordTPrimary { getDiscordTPrimary :: UserId }
    deriving newtype (QueryParams, QueryResults)
  tableName = "discord"

instance InTable DiscordTable (PrimaryKey DiscordTable) where
  columnRep = ColRep [SomeCol DiscordTId]

type instance MainTable (PrimaryKey DiscordTable) = DiscordTable