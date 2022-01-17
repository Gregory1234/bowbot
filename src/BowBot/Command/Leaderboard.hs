{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module BowBot.Command.Leaderboard where

import BowBot.Stats.HypixelBow
import BowBot.Command
import BowBot.Minecraft
import Data.List (sortOn, find)
import Data.List.Split (chunksOf)
import Data.Maybe (listToMaybe)

data LeaderboardElement = LeaderboardElement { lbPos :: Integer, lbName :: String, lbVal :: String, lbUUID :: UUID } deriving Show

data LeaderboardPage = LeaderboardPage Int | LeaderboardAll | LeaderboardSearch UUID deriving Show

hypixelBowLeaderboardCommand :: String -> String -> String -> (HypixelBowLeaderboards -> Maybe (Integer, String)) -> Command
hypixelBowLeaderboardCommand name lbname statname lbfun = Command name DefaultLevel 10 $ do
  caller <- hCaller
  dat <- getHypixelBowLeaderboard
  let lb = [(val, (uuid, str)) | (uuid, lbv) <- toList dat, Just (val, str) <- [lbfun lbv]]
  sortedlb <- for (sortOn (negate . fst) lb) $ \(_, (uuid, str)) -> do
    names <- fromMaybe [] <$> mcUUIDToNames uuid
    return (head names, str, uuid)
  let elems = zipWith (\lbPos (lbName, lbVal, lbUUID) -> LeaderboardElement {..}) [1..] sortedlb
  args <- hArgs
  selectedOrMsg <- case args of
    [] -> (\x -> case find (`elem` x) (map lbUUID elems) of 
                    Just pl -> Right (x, Nothing, LeaderboardSearch pl)
                    Nothing -> Right ([], if null x then Nothing else Just "*You are not on this leaderboard:*", LeaderboardPage 0)
          ) <$> getAuthorNicks (userId caller)
    ["all"] -> Right . (,Nothing, LeaderboardAll) <$> getAuthorNicks (userId caller)
    [readMaybe -> Just page] -> Right . (,Nothing, LeaderboardPage (page - 1)) <$> getAuthorNicks (userId caller)
    [mcName] -> do
      res <- withMinecraftAutocorrect True mcName $ \uuid _ ->
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
   getAuthorNicks did = do
     accounts <- hRead bowBotAccounts
     return $ [mc | BowBotAccount {..} <- accounts, did `elem` accountDiscords, mc <- accountMinecrafts]


generateLeaderboard :: String -> [LeaderboardElement] -> [UUID] -> [String]
generateLeaderboard statname lb selected = map helper lb
  where
    helper LeaderboardElement {..}
      = pad 5 (show lbPos ++ ".")
      ++ (if lbUUID `elem` selected then "*" else " ")
      ++ pad 20 lbName
      ++ " ( " ++ lbVal ++ " " ++ statname ++ " )"