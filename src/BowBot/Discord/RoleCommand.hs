{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module BowBot.Discord.RoleCommand where

import BowBot.Command
import qualified Data.Map as M
import qualified Discord.Requests as R
import BowBot.BotData.Info
import BowBot.Discord.SavedRoles
import BowBot.Discord.Utils
import qualified Data.Text as T

roleCommand :: Command
roleCommand = Command CommandInfo
  { commandName = "role"
  , commandHelpEntries = 
    [ HelpEntry { helpUsage = "role", helpDescription = "show all toggleable roles", helpGroup = "normal" }
    , HelpEntry { helpUsage = "role [role name]", helpDescription = "toggle a discord role", helpGroup = "normal" } ]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOptionalArgument (traverse $ \n -> fmap (,n) . liftMaybe "*Toggleable role not found!*" . (M.!? n) . M.mapKeys savedRoleName =<< askInfo toggleableRolesInfo) $ \case
    Nothing -> do
      allRoles <- M.mapKeys savedRoleName <$> askInfo toggleableRolesInfo
      gid <- askInfo discordGuildIdInfo
      sender <- userId <$> envs envSender
      maybeMem <- call $ R.GetGuildMember gid sender
      case maybeMem of
        Left _ -> respond somethingWentWrongMessage
        Right mem -> do
          maybeRoles <- call $ R.GetGuildRoles gid
          case maybeRoles of
            Left _ -> respond somethingWentWrongMessage
            Right roleList -> do
              respond $ "Toggleable roles: ```\n" <> T.intercalate ", " (map (\(n,r) ->
                (if r `elem` memberRoles mem then "*" else "") <> n
                  <> " (@" <> head (roleName <$> filter ((==r) . roleId) roleList) <> ")") (M.toList allRoles)
                ) <> "```"
    Just (rid, role) -> do
      gid <- askInfo discordGuildIdInfo
      sender <- userId <$> envs envSender
      maybeMem <- call $ R.GetGuildMember gid sender
      case maybeMem of
        Left _ -> respond somethingWentWrongMessage
        Right mem -> do
          if rid `notElem` memberRoles mem
          then do
            call_ $ R.AddGuildMemberRole gid sender rid
            respond $ "Added role " <> role
          else do
            call_ $ R.RemoveGuildMemberRole gid sender rid
            respond $ "Removed role " <> role