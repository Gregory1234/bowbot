{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module BowBot.Command.Leaderboard where

import BowBot.Stats
import BowBot.Command
import BowBot.Minecraft
import Data.Map (toList)
import Data.List (sortOn)
import Data.List.Split (chunksOf)

data LeaderboardElement = LeaderboardElement { lbPos :: Integer, lbName :: String, lbVal :: String, lbUUID :: String } deriving Show

data LeaderboardPage = LeaderboardPage Int | LeaderboardAll | LeaderboardSearch String deriving Show

leaderboardCommand :: StatType s => Proxy s -> String -> String -> String -> (Leaderboards s -> Maybe (Integer, String)) -> Command
leaderboardCommand pr name lbname statname lbfun = Command name DefaultLevel 10 $ do
  man <- hManager
  bdt <- hData
  caller <- hCaller
  maybedat <- liftIO $ getLeaderboard pr man
  case maybedat of
    Nothing -> hRespond somethingWrongMessage
    Just dat -> do
      let lb = [(val, (uuid, str)) | (uuid, lbv) <- toList dat, Just (val, str) <- [lbfun lbv]]
      sortedlb <- liftIO $ for (sortOn (negate . fst) lb) $ \(_, (uuid, str)) -> do
        names <- fromMaybe [] <$> mcUUIDToNames man bdt uuid
        return (head names, str, uuid)
      let elems = zipWith (\lbPos (lbName, lbVal, lbUUID) -> LeaderboardElement {..}) [1..] sortedlb
      args <- hArgs
      selectedOrMsg <- case args of
        [] -> stm $ (\x -> Right (x ,Nothing, LeaderboardSearch (head $ filter (`elem` x) $ map lbUUID elems))) <$> getAuthorNicks bdt (userId caller)
        ["all"] -> stm $ Right . (,Nothing, LeaderboardAll) <$> getAuthorNicks bdt (userId caller)
        [readMaybe -> Just page] -> stm $ Right . (,Nothing, LeaderboardPage (page - 1)) <$> getAuthorNicks bdt (userId caller)
        [mcName] -> do
          res <- withMinecraftAutocorrect man bdt False mcName $ \uuid _ ->
            return $ if uuid `elem` map lbUUID elems then Right uuid else Left ()
          return $ case res of
            PlayerNotFound -> Left playerNotFoundMessage
            DiscordUserNotFound -> Left discordNotFoundMessage
            (UserError ()) -> Left "*The player isn't on this leaderboard!*"
            NotOnList -> Left registerMessage
            JustResponse _ u -> Right ([u], Nothing, LeaderboardSearch u)
            OldResponse _ _ u -> Right ([u], Nothing, LeaderboardSearch u)
            DidYouMeanResponse n u -> Right ([u], Just $ "*Did you mean* **" ++ n ++ "**:\n", LeaderboardSearch u)
            DidYouMeanOldResponse o n u -> Right ([u], Just $ "*Did you mean* **" ++ o ++ " (" ++ n ++ "):**\n", LeaderboardSearch u)
        _ -> return $ Left wrongSyntaxMessage
      case selectedOrMsg of
        Left msg -> hRespond msg
        Right (selected, msg, lbPage) -> do
          let lbFull = generateLeaderboard statname elems selected
          let pages = chunksOf 20 lbFull
          case lbPage of
            LeaderboardPage page ->
              if page < 0 || page >= length pages
              then hRespond $ "*Wrong page number, it has to be between **1** and **" ++ show (length pages) ++ "**.*"
              else hRespond $ lbname ++ " (page **" ++ show (page + 1) ++ "/" ++ show (length pages) ++ "**):\n" ++ fromMaybe "" msg ++ "```\n" ++ unlines (pages !! page) ++ "```"
            LeaderboardSearch uuid -> do
              let searched = head $ filter ((==uuid) . lbUUID) elems
              let page = fromIntegral $ (lbPos searched - 1) `div` 20
              hRespond $ lbname ++ " (page **" ++ show (page + 1) ++ "/" ++ show (length pages) ++ "**):\n" ++ fromMaybe "" msg ++ "```\n" ++ unlines (pages !! page) ++ "```"
            LeaderboardAll -> do
              for_ msg hRespond
              hRespondFile (pack $ statname ++ ".txt") $ unlines lbFull
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