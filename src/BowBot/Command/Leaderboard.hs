{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module BowBot.Command.Leaderboard where

import BowBot.Stats
import BowBot.Command
import BowBot.Minecraft
import Data.Proxy (Proxy)
import Discord
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)
import Data.Text (unpack, pack)
import Discord.Types
import Data.Map (toList)
import Data.List (sortOn)
import Data.Traversable (for)
import Data.Maybe (fromMaybe)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (readTVar)
import BowBot.BotData
import BowBot.Utils
import Text.Read (readMaybe)
import Data.List.Split (chunksOf)
import Data.Foldable (for_)

data LeaderboardElement = LeaderboardElement { lbPos :: Integer, lbName :: String, lbVal :: String, lbUUID :: String } deriving Show

data LeaderboardPage = LeaderboardPage Int | LeaderboardAll | LeaderboardSearch String

leaderboardCommand :: StatType s => Proxy s -> String -> String -> String -> (Leaderboards s -> Maybe (Integer, String)) -> Command
leaderboardCommand pr name lbname statname lbfun = Command name 2 $ \m man bdt -> do
  maybedat <- liftIO $ getLeaderboard pr man
  case maybedat of
    Nothing -> respond m ""
    Just dat -> do
      let lb = [(val, (uuid, str)) | (uuid, lbv) <- toList dat, Just (val, str) <- [lbfun lbv]]
      sortedlb <- liftIO $ for (sortOn (negate . fst) lb) $ \(_, (uuid, str)) -> do
        names <- fromMaybe [] <$> mcUUIDToNames man bdt uuid
        return (head names, str, uuid)
      let elems = zipWith (\lbPos (lbName, lbVal, lbUUID) -> LeaderboardElement {..}) [1..] sortedlb
      let args = words $ dropWhile isSpace $ dropWhile (not . isSpace) $ unpack (messageText m)
      selectedOrMsg <- case args of
        [] -> liftIO $ atomically $ Right . (,Nothing, LeaderboardPage 0) <$> getAuthorNicks bdt (userId $ messageAuthor m)
        ["all"] -> liftIO $ atomically $ Right . (,Nothing, LeaderboardAll) <$> getAuthorNicks bdt (userId $ messageAuthor m)
        [readMaybe -> Just page] -> liftIO $ atomically $ Right . (,Nothing, LeaderboardPage (page - 1)) <$> getAuthorNicks bdt (userId $ messageAuthor m)
        [mcName] -> do
          res <- withMinecraft man bdt True (Left mcName) $ \uuid _ -> do
            liftIO $ print uuid
            return $ if uuid `elem` map lbUUID elems then Just uuid else Nothing
          return $ case res of
            NoResponse -> Left "*The player doesn't exist or isn't on this leaderboard!*"
            JustResponse _ u -> Right ([u], Nothing, LeaderboardSearch u)
            OldResponse _ _ u -> Right ([u], Nothing, LeaderboardSearch u)
            DidYouMeanResponse n u -> Right ([u], Just $ "*Did you mean* **" ++ n ++ "**:\n", LeaderboardSearch u)
            DidYouMeanOldResponse o n u -> Right ([u], Just $ "*Did you mean* **" ++ o ++ " (" ++ n ++ "):**\n", LeaderboardSearch u)
            NotOnList -> Left registerMessage
        _ -> return $ Left "*Wrong command syntax*"
      case selectedOrMsg of
        Left msg -> respond m msg
        Right (selected, msg, lbPage) -> do
          let lbFull = generateLeaderboard statname elems selected
          let pages = chunksOf 20 lbFull
          case lbPage of
            LeaderboardPage page ->
              if page < 0 || page >= length pages
              then respond m $ "*Wrong page number, it has to be between **1** and **" ++ show (length pages) ++ "**.*"
              else respond m $ lbname ++ " (page **" ++ show (page + 1) ++ "**):\n" ++ fromMaybe "" msg ++ "```\n" ++ unlines (pages !! page) ++ "```"
            LeaderboardSearch uuid -> do
              let searched = head $ filter ((==uuid) . lbUUID) elems
              let page = fromIntegral $ (lbPos searched - 1) `div` 20
              respond m $ lbname ++ " (page **" ++ show (page + 1) ++ "**):\n" ++ fromMaybe "" msg ++ "```\n" ++ unlines (pages !! page) ++ "```"
            LeaderboardAll -> do
              for_ msg $ respond m
              respondFile m (pack $ statname ++ ".txt") $ unlines lbFull
 where
   getAuthorNicks bdt did = do
     accounts <- readTVar $ bowBotAccounts bdt
     return $ [mc | BowBotAccount {..} <- accounts, did `elem` accountDiscords, mc <- accountMinecrafts]


generateLeaderboard :: String -> [LeaderboardElement] -> [String] -> [String]
generateLeaderboard statname lb selected = map helper lb
  where
    helper LeaderboardElement {..}
      = pad 5 (show lbPos ++ ".")
      ++ (if lbUUID `elem` selected then "*" else " ")
      ++ pad 20 lbName
      ++ " ( " ++ lbVal ++ " " ++ statname ++ " )"