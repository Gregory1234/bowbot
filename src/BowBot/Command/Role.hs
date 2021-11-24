module BowBot.Command.Role where

import BowBot.Command
import qualified Discord.Requests as R
import Data.Char (isSpace)

roleCommand :: Command
roleCommand = Command "role" DefaultLevel 10 $ \m _ bdt -> do
  let args = words $ dropWhile isSpace $ dropWhile (not . isSpace) $ unpack (messageText m)
  case args of
    [] -> do
      allRoles <- readProp discordToggleableRoles bdt
      respond m $ "Toggleable roles: ```\n" ++ unwords (map fst allRoles) ++ "```"
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