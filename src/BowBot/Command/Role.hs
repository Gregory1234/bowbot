module BowBot.Command.Role where

import BowBot.Command
import qualified Discord.Requests as R
import Data.Char (isSpace)
import Data.List (intercalate)

roleCommand :: Command
roleCommand = Command "role" DefaultLevel 10 $ \m _ bdt -> do
  let args = words $ dropWhile isSpace $ dropWhile (not . isSpace) $ unpack (messageText m)
  case args of
    [] -> do
      allRoles <- readProp discordToggleableRoles bdt
      gid <- readProp discordGuildId bdt
      maybeMem <- call $ R.GetGuildMember gid (userId $ messageAuthor m)
      case maybeMem of
        Left _ -> respond m somethingWrongMessage
        Right mem -> do
          maybeRoles <- call $ R.GetGuildRoles gid
          case maybeRoles of
            Left _ -> respond m somethingWrongMessage
            Right roleList -> do
              respond m $ "Toggleable roles: ```\n" ++ intercalate ", " (map (\(n,r) ->
                (if r `elem` memberRoles mem then "*" else "") ++ n
                  ++ " (@" ++ head (unpack . roleName <$> filter ((==r) . roleId) roleList) ++ ")") allRoles
                ) ++ "```"
    [role] -> do
      allRoles <- readProp discordToggleableRoles bdt
      case lookup role allRoles of
        Nothing -> respond m $ "Toggleable role " ++ role ++ " not found!"
        Just rid -> do
          gid <- readProp discordGuildId bdt
          maybeMem <- call $ R.GetGuildMember gid (userId $ messageAuthor m)
          case maybeMem of
            Left _ -> respond m somethingWrongMessage
            Right mem -> do
              if rid `notElem` memberRoles mem
              then do
                call_ $ R.AddGuildMemberRole gid (userId $ messageAuthor m) rid
                respond m $ "Added role " ++ role
              else do
                call_ $ R.RemoveGuildMemberRole gid (userId $ messageAuthor m) rid
                respond m $ "Removed role " ++ role
    _ -> respond m wrongSyntaxMessage