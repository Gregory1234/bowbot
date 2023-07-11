{-# LANGUAGE TypeFamilies #-}

module BowBot.Perms.Table where

import BowBot.Discord.Utils
import BowBot.DB.Typed
import BowBot.Perms.Type

data PermsTable a where
  PermsTDiscord :: PermsTable UserId
  PermsTLevel :: PermsTable PermissionLevel

instance DatabaseTableLike PermsTable where
  columnName PermsTDiscord = "discord_id"
  columnName PermsTLevel = "level"

instance DatabaseTable PermsTable where
  newtype PrimaryKey PermsTable = PermsTPrimary { getPermsTPrimary :: UserId }
    deriving newtype (QueryParams, QueryResults)
  tableName = "permissions"

instance InTable PermsTable (PrimaryKey PermsTable) where
  columnRep = ColRep [SomeCol PermsTDiscord]

type instance MainTable (PrimaryKey PermsTable) = PermsTable