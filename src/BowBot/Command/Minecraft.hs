{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module BowBot.Command.Minecraft where

import BowBot.Command
import BowBot.Minecraft

minecraftCommand :: Command
minecraftCommand = Command "mc" DefaultLevel 2 $ do
  caller <- hCaller
  args <- hArgs
  pns <- fmap (>>=(\account@BowBotAccount {..} -> (,account) <$> accountDiscords)) $ hRead bowBotAccounts
  case lookup (userId caller) pns of
    Nothing -> case args of
      [readMaybe . filter (`notElem` "<@!>") -> Just did] -> case lookup did pns of
        Nothing -> hRespond discordNotFoundMessage
        Just bac -> do
          mcList <- for (accountMinecrafts bac) $ \x -> do
            name <- maybe undefined head <$> mcUUIDToNames x
            return $ (if accountSelectedMinecraft bac == x then "*" else "") ++ name
          hRespond $ "**List of minecraft nicks:**\n```\n" ++ unlines mcList ++ "```"
      [_] -> hRespond discordNotFoundMessage
      _ -> hRespond registerMessage
    Just bac -> case args of
      [] -> do
        mcList <- for (accountMinecrafts bac) $ \x -> do
          name <- maybe undefined head <$> mcUUIDToNames x
          return $ (if accountSelectedMinecraft bac == x then "*" else "") ++ name
        hRespond $ "**List of your minecraft nicks linked:**\n```\n" ++ unlines mcList ++ "```"
      [readMaybe . filter (`notElem` "<@!>") -> Just did] -> case lookup did pns of
        Nothing -> hRespond discordNotFoundMessage
        Just bac' -> do -- TODO: remove repetition
          mcList <- for (accountMinecrafts bac') $ \x -> do
            name <- maybe undefined head <$> mcUUIDToNames x
            return $ (if accountSelectedMinecraft bac' == x then "*" else "") ++ name
          hRespond $ "**List of minecraft nicks:**\n```\n" ++ unlines mcList ++ "```"
      [mc] -> do -- TODO: add autocorrect for ?mc
        maybeUUID <- mcNameToUUID mc
        case maybeUUID of
          Nothing -> hRespond playerNotFoundMessage
          Just mcUUID -> if mcUUID `elem` accountMinecrafts bac
            then do
              _ <- hSendDB "people/select.php" ["id=" ++ show (accountId bac), "minecraft=" ++ uuidString mcUUID]
              hModify bowBotAccounts $ map (\u -> if accountId bac == accountId u then u { accountSelectedMinecraft = mcUUID } else u)
              hRespond "*Success!*"
            else
              hRespond "*You do not have that minecraft nick registered! If this is your alt, ask someone to add it.*"
      _ -> hRespond wrongSyntaxMessage