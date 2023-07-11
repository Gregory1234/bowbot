{-# LANGUAGE TypeFamilies #-}

module BowBot.Perms.Basic(
  module BowBot.Perms.Basic, module BowBot.Perms.Type
) where

import Discord.Types (UserId)
import BowBot.Discord.Orphans ()
import BowBot.Utils
import BowBot.Perms.Type
import BowBot.Perms.Table
import BowBot.DB.Typed

instance InTable PermsTable PermissionLevel where
  columnRep = ColRep [SomeCol PermsTLevel]

type instance MainTable PermissionLevel = PermsTable

getPermissionLevelByDiscord :: (MonadIOReader m r, Has Connection r) => UserId -> m PermissionLevel
getPermissionLevelByDiscord discord = fromMaybe DefaultLevel <$> queryOnlyLogT selectByPrimaryQuery (PermsTPrimary discord)