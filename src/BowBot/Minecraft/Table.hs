{-# LANGUAGE TypeFamilies #-}

module BowBot.Minecraft.Table where

import BowBot.Utils
import BowBot.DB.Typed
import BowBot.Minecraft.Basic
import BowBot.Minecraft.IsBanned
import BowBot.Hypixel.Guild.Role

data MinecraftTable a where
  MinecraftTUUID :: MinecraftTable UUID
  MinecraftTNames :: MinecraftTable Text
  MinecraftTHypixel :: MinecraftTable IsBanned
  MinecraftTHypixelRole :: MinecraftTable HypixelRole

instance DatabaseTableLike MinecraftTable where
  columnName MinecraftTUUID = "uuid"
  columnName MinecraftTNames = "names"
  columnName MinecraftTHypixel = "hypixel"
  columnName MinecraftTHypixelRole = "hypixel_role"

instance DatabaseTable MinecraftTable where
  newtype PrimaryKey MinecraftTable = MinecraftTPrimary { getMinecraftTPrimary :: UUID }
    deriving newtype (QueryParams, QueryResults)
  tableName = "minecraft"

instance InTable MinecraftTable (PrimaryKey MinecraftTable) where
  columnRep = ColRep [SomeCol MinecraftTUUID]

type instance MainTable (PrimaryKey MinecraftTable) = MinecraftTable