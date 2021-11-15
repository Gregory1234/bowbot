{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module BowBot.Command.Minecraft where

import BowBot.Command
import BowBot.Minecraft
import BowBot.API
import Data.Char (isSpace)

minecraftCommand :: Command
minecraftCommand = Command "mc" DefaultLevel 2 $ \m man bdt -> do
  let args = words $ dropWhile isSpace $ dropWhile (not . isSpace) $ unpack (messageText m)
  pns <- fmap (>>=(\account@BowBotAccount {..} -> (,account) <$> accountDiscords)) $ readProp bowBotAccounts bdt
  case lookup (userId $ messageAuthor m) pns of
    Nothing -> case args of
      [readMaybe . filter (`notElem` "<@!>") -> Just did] -> case lookup did pns of
        Nothing -> respond m discordNotFoundMessage
        Just bac -> do
          mcList <- for (accountMinecrafts bac) $ \x -> do
            name <- liftIO $ maybe undefined head <$> mcUUIDToNames man bdt x
            return $ (if accountSelectedMinecraft bac == x then "*" else "") ++ name
          respond m $ "**List of minecraft nicks:**\n```\n" ++ unlines mcList ++ "```"
      [_] -> respond m discordNotFoundMessage
      _ -> respond m registerMessage
    Just bac -> case args of
      [] -> do
        mcList <- for (accountMinecrafts bac) $ \x -> do
          name <- liftIO $ maybe undefined head <$> mcUUIDToNames man bdt x
          return $ (if accountSelectedMinecraft bac == x then "*" else "") ++ name
        respond m $ "**List of your minecraft nicks linked:**\n```\n" ++ unlines mcList ++ "```"
      [readMaybe . filter (`notElem` "<@!>") -> Just did] -> case lookup did pns of
        Nothing -> respond m discordNotFoundMessage
        Just bac' -> do -- TODO: remove repetition
          mcList <- for (accountMinecrafts bac') $ \x -> do
            name <- liftIO $ maybe undefined head <$> mcUUIDToNames man bdt x
            return $ (if accountSelectedMinecraft bac' == x then "*" else "") ++ name
          respond m $ "**List of minecraft nicks:**\n```\n" ++ unlines mcList ++ "```"
      [mc] -> do -- TODO: add autocorrect for ?mc
        maybeUUID <- liftIO $ mcNameToUUID man bdt mc
        case maybeUUID of
          Nothing -> respond m playerNotFoundMessage
          Just mcUUID -> if mcUUID `elem` accountMinecrafts bac
            then do
              _ <- liftIO $ sendDB man "people/select.php" ["id=" ++ show (accountId bac), "minecraft=" ++ mcUUID]
              modifyProp bowBotAccounts bdt $ map (\u -> if accountId bac == accountId u then u { accountSelectedMinecraft = mcUUID } else u)
              respond m "*Success!*"
            else
              respond m "*You do not have that minecraft nick registered! If this is your alt, ask someone to add it.*"
      _ -> respond m wrongSyntaxMessage