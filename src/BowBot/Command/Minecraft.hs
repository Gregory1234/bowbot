{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module BowBot.Command.Minecraft where

import BowBot.Command
import BowBot.BotData
import BowBot.Minecraft
import BowBot.API
import Data.Char (isSpace)
import Data.Text (unpack)
import Discord.Types hiding (accountId)
import Control.Monad.Cont (liftIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (readTVar, writeTVar, modifyTVar)
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Network.HTTP.Conduit (Manager)

minecraftCommand :: Command
minecraftCommand = Command "mc" DefaultLevel 2 $ \m man bdt -> do
  let args = words $ dropWhile isSpace $ dropWhile (not . isSpace) $ unpack (messageText m)
  pns <- fmap (>>=(\account@BowBotAccount {..} -> (,account) <$> accountDiscords)) $ liftIO $ atomically $ readTVar (bowBotAccounts bdt)
  case lookup (userId $ messageAuthor m) pns of
    Nothing -> respond m registerMessage
    Just bac -> case args of
      [] -> do
        mcList <- for (accountMinecrafts bac) $ \x -> do
          name <- liftIO $ maybe undefined head <$> mcUUIDToNames man bdt x
          return $ (if accountSelectedMinecraft bac == x then "*" else "") ++ name
        respond m $ "**List of your minecraft nicks linked:**\n```\n" ++ unlines mcList ++ "```"
      [mc] -> do -- TODO: add autocorrect for ?mc
        maybeUUID <- liftIO $ mcNameToUUID man bdt mc
        case maybeUUID of
          Nothing -> respond m playerNotFoundMessage
          Just mcUUID -> if mcUUID `elem` accountMinecrafts bac
            then do
              _ <- liftIO $ sendDB man "people/select.php" ["id=" ++ show (accountId bac), "minecraft=" ++ mcUUID]
              liftIO $ atomically $ modifyTVar (bowBotAccounts bdt) $ map (\u -> if accountId bac == accountId u then u { accountSelectedMinecraft = mcUUID } else u)
              respond m "*Success!*"
            else
              respond m "*You do not have that minecraft nick registered! If this is your alt, ask someone to add it.*"
      _ -> respond m wrongSyntaxMessage