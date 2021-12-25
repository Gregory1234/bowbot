module BowBot.Command.Role where

import BowBot.Command
import qualified Discord.Requests as R
import Data.List (intercalate)

roleCommand :: Command
roleCommand = Command "role" DefaultLevel 10 $ do
  caller <- hCaller
  args <- hArgs
  case args of
    [] -> do
      allRoles <- hRead discordToggleableRoles
      gid <- hRead discordGuildId
      maybeMem <- hDiscord $ call $ R.GetGuildMember gid (userId caller)
      case maybeMem of
        Left _ -> hRespond somethingWrongMessage
        Right mem -> do
          maybeRoles <- hDiscord $ call $ R.GetGuildRoles gid
          case maybeRoles of
            Left _ -> hRespond somethingWrongMessage
            Right roleList -> do
              hRespond $ "Toggleable roles: ```\n" ++ intercalate ", " (map (\(n,r) ->
                (if r `elem` memberRoles mem then "*" else "") ++ n
                  ++ " (@" ++ head (unpack . roleName <$> filter ((==r) . roleId) roleList) ++ ")") allRoles
                ) ++ "```"
    [role] -> do
      allRoles <- hRead discordToggleableRoles
      case lookup role allRoles of
        Nothing -> hRespond $ "Toggleable role " ++ role ++ " not found!"
        Just rid -> do
          gid <- hRead discordGuildId
          maybeMem <- hDiscord $ call $ R.GetGuildMember gid (userId caller)
          case maybeMem of
            Left _ -> hRespond somethingWrongMessage
            Right mem -> do
              if rid `notElem` memberRoles mem
              then do
                hDiscord $ call_ $ R.AddGuildMemberRole gid (userId caller) rid
                hRespond $ "Added role " ++ role
              else do
                hDiscord $ call_ $ R.RemoveGuildMemberRole gid (userId caller) rid
                hRespond $ "Removed role " ++ role
    _ -> hRespond wrongSyntaxMessage