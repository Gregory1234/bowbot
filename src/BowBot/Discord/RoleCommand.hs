{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module BowBot.Discord.RoleCommand where

import BowBot.Command
import qualified Data.Map as M
import qualified Discord.Requests as R
import BowBot.BotData.Info
import BowBot.Discord.Roles
import BowBot.Discord.Utils
import Data.List (intercalate)

roleCommand :: Command
roleCommand = Command CommandInfo
  { commandName = "role"
  , commandUsage = "role [role name|]"
  , commandDescription = "show all toggleable roles / toggle a discord role"
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  , commandGroup = "normal"
  } $ hOneOptionalArgument (traverse $ \n -> fmap (,n) . liftMaybe "*Toggleable role not found!*" . (M.!? n) =<< hInfoDB toggleableRolesInfo) $ \case
    Nothing -> do
      allRoles <- hInfoDB toggleableRolesInfo
      gid <- hInfoDB discordGuildIdInfo
      sender <- userId <$> hEnv envSender
      maybeMem <- call $ R.GetGuildMember gid sender
      case maybeMem of
        Left _ -> hRespond somethingWentWrongMessage
        Right mem -> do
          maybeRoles <- call $ R.GetGuildRoles gid
          case maybeRoles of
            Left _ -> hRespond somethingWentWrongMessage
            Right roleList -> do
              hRespond $ "Toggleable roles: ```\n" ++ intercalate ", " (map (\(n,r) ->
                (if r `elem` memberRoles mem then "*" else "") ++ n
                  ++ " (@" ++ head (unpack . roleName <$> filter ((==r) . roleId) roleList) ++ ")") (M.toList allRoles)
                ) ++ "```"
    Just (rid, role) -> do
      gid <- hInfoDB discordGuildIdInfo
      sender <- userId <$> hEnv envSender
      maybeMem <- call $ R.GetGuildMember gid sender
      case maybeMem of
        Left _ -> hRespond somethingWentWrongMessage
        Right mem -> do
          if rid `notElem` memberRoles mem
          then do
            call_ $ R.AddGuildMemberRole gid sender rid
            hRespond $ "Added role " ++ role
          else do
            call_ $ R.RemoveGuildMemberRole gid sender rid
            hRespond $ "Removed role " ++ role