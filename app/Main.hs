{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Applicative ((<|>))
import Control.Concurrent (forkFinally, forkIO, threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM
import Control.Monad (forever, unless, void, when, join)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes, fromMaybe, mapMaybe, listToMaybe, maybeToList)
import Data.Ratio ((%))
import Data.Text (Text, isPrefixOf, pack, strip, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock
import Data.Time.Format (defaultTimeLocale, formatTime)
import Discord
import qualified Discord.Requests as R
import Discord.Types
import Network.HTTP.Conduit
import System.Environment.Blank (getEnv)
import System.Timeout (timeout)
import Text.Printf (printf)
import Data.Char (toLower)

data BowBotData = BowBotData
  { count :: TVar Int,
    countBorder :: TVar Int,
    nickCache :: TVar [(String, [String])],
    online :: TVar (Maybe [String]),
    onlineBorder :: TVar (Maybe [String]),
    onlineBusy :: TVar Bool
  }

main :: IO ()
main = do
  apiKey <- fromMaybe "" <$> getEnv "API_KEY"
  unless (apiKey == "") $ do
    count <- atomically $ newTVar 0
    countBorder <- atomically $ newTVar 0
    f <- readFile "nicks.txt"
    nickCache <- atomically $ newTVar $ (,[]) <$> lines f
    online <- atomically $ newTVar Nothing
    onlineBorder <- atomically $ newTVar Nothing
    onlineBusy <- atomically $ newTVar False
    let bbdata = BowBotData {..}
    updateNicks nickCache
    mkBackground bbdata
    forever $ do
      userFacingError <-
        runDiscord $
          def
            { discordToken = pack apiKey,
              discordOnEvent = eventHandler bbdata
            }
      TIO.putStrLn userFacingError

updateNicks :: TVar [(String, [String])] -> IO ()
updateNicks nickCache = do
  manager <- newManager tlsManagerSettings
  updatedNicks <- mapConcurrently (\(u, _) -> (u,) <$> uuidToNames manager u) =<< atomically (readTVar nickCache)
  atomically $ writeTVar nickCache updatedNicks

mkBackground :: BowBotData -> IO ()
mkBackground bbdata = void $
  forkFinally (background bbdata) $ \e -> do
    print e
    mkBackground bbdata

setAt :: Int -> a -> [a] -> [a]
setAt i a ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 (_ : xs) = a : xs
    go n (x : xs) = x : go (n -1) xs
    go _ [] = []
{-# INLINE setAt #-}

getTime :: String -> IO String
getTime f = formatTime defaultTimeLocale f <$> getCurrentTime

background :: BowBotData -> IO ()
background BowBotData {..} = do
  sec <- read @Int <$> getTime "%S"
  threadDelay ((65 - sec `mod` 60) * 1000000)
  forever go
  where
    go = do
      _ <- timeout 1000000 . forkIO $ do
        mint <- getTime "%M"
        putStrLn "New minute!"
        atomically $ do
          bc <- readTVar countBorder
          writeTVar count bc
          writeTVar countBorder 0
        atomically $ do
          onl <- readTVar onlineBorder
          writeTVar online onl
          writeTVar onlineBorder Nothing
        when (mint == "00") $ updateNicks nickCache
        putStrLn "New minute finished!"
      threadDelay 60000000

sendRequestTo :: Manager -> String -> IO ByteString
sendRequestTo manager url = do
  putStrLn url
  request <- parseRequest url
  res <- httpLbs request manager
  return $ responseBody res

data Stats = Stats
  { playerName :: String,
    bowWins :: Integer,
    bowLosses :: Integer,
    bestWinstreak :: Integer,
    currentWinstreak :: Integer
  }
  deriving (Show)

getStats :: Manager -> String -> IO (Maybe Stats)
getStats manager uuid = do
  apiKey <- fromMaybe "" <$> getEnv "HYPIXEL_API"
  let url = "https://api.hypixel.net/player?key=" ++ apiKey ++ "&uuid=" ++ uuid
  res <- sendRequestTo manager url
  case decode res :: Maybe Object of
    Nothing -> return Nothing
    (Just js) -> do
      putStrLn $ "Received response from: " ++ url
      case js HM.!? "player" of
        (Just (Object pl)) -> case (pl HM.!? "displayname", pl HM.!? "stats") of
          (Just (String (unpack -> playerName)), Just (Object st)) -> case st HM.!? "Duels" of
            (Just (Object ds)) -> case (ds HM.!? "current_bow_winstreak", ds HM.!? "best_bow_winstreak", ds HM.!? "bow_duel_wins", ds HM.!? "bow_duel_losses") of
              (Just (Number (round -> currentWinstreak)), Just (Number (round -> bestWinstreak)), Just (Number (round -> bowWins)), Just (Number (round -> bowLosses))) ->
                return . Just $ Stats {..}
              (Just (Number (round -> currentWinstreak)), Just (Number (round -> bestWinstreak)), Just (Number (round -> bowWins)), Nothing) ->
                return . Just $ Stats {bowLosses = 0, ..}
              (_, _, Nothing, Just (Number (round -> bowLosses))) ->
                return . Just $ Stats {bowWins = 0, currentWinstreak = 0, bestWinstreak = 0, ..}
              (_, _, Nothing, Nothing) ->
                return . Just $ Stats {bowWins = 0, bowLosses = 0, currentWinstreak = 0, bestWinstreak = 0, ..}
              _ -> return Nothing
            _ -> return . Just $ Stats {bowWins = 0, bowLosses = 0, currentWinstreak = 0, bestWinstreak = 0, ..}
          _ -> return Nothing
        _ -> return Nothing

isInBowDuels :: Manager -> String -> IO (Maybe Bool)
isInBowDuels manager uuid = do
  apiKey <- fromMaybe "" <$> getEnv "HYPIXEL_API"
  let url = "https://api.hypixel.net/status?key=" ++ apiKey ++ "&uuid=" ++ uuid
  res <- sendRequestTo manager url
  case decode res :: Maybe Object of
    Nothing -> return Nothing
    (Just js) -> do
      putStrLn $ "Received response from: " ++ url
      case js HM.!? "session" of
        (Just (Object ses)) -> case ses HM.!? "mode" of
          (Just (String name)) -> return . Just $ name == "BOW_DUEL"
          _ -> return $ Just False
        _ -> return $ Just False

showStats :: Stats -> String
showStats Stats {..} =
  "**" ++ playerName ++ ":**\n"
    ++ "- *Bow Duels Wins:* **"
    ++ show bowWins
    ++ "**\n"
    ++ " - *Bow Duels Losses:* **"
    ++ show bowLosses
    ++ "**\n"
    ++ " - *Bow Duels Win/Loss Ratio:* **"
    ++ winLossRatio
    ++ "**\n"
    ++ " - *Bow Duels Wins until "
    ++ nextWinLossRatio
    ++ " WLR:* **"
    ++ winsRemaining
    ++ "**\n"
    ++ " - *Best Bow Duels Winstreak:* **"
    ++ show bestWinstreak
    ++ "**\n"
    ++ " - *Current Bow Duels Winstreak:* **"
    ++ show currentWinstreak
    ++ "**"
  where
    winLossRatio
      | bowWins == 0, bowLosses == 0 = "NaN"
      | bowLosses == 0 = "∞"
      | otherwise = printf "%.04f" (fromRational (bowWins % bowLosses) :: Double)
    nextWinLossRatio
      | bowLosses == 0 = "∞"
      | otherwise = show $ (bowWins `div` bowLosses) + 1
    winsRemaining
      | bowWins == 0, bowLosses == 0 = "1"
      | bowLosses == 0 = "N/A"
      | otherwise = show (bowLosses - (bowWins `mod` bowLosses))

nameToUUID :: Manager -> String -> IO (Maybe String)
nameToUUID manager name = do
  let url = "https://api.mojang.com/users/profiles/minecraft/" ++ name
  res <- sendRequestTo manager url
  case decode res :: Maybe Object of
    Nothing -> return Nothing
    (Just js) -> do
      putStrLn $ "Received response from: " ++ url
      case js HM.!? "id" of
        (Just (String text)) -> do
          return . Just $ unpack text
        _ -> return Nothing

uuidToNames :: Manager -> String -> IO [String]
uuidToNames manager uuid = do
  let url = "https://api.mojang.com/user/profiles/" ++ uuid ++ "/names"
  res <- sendRequestTo manager url
  case decode res :: Maybe [Object] of
    Nothing -> return []
    (Just js) -> do
      putStrLn $ "Received response from: " ++ url
      return . mapMaybe helper $ reverse js
  where
    helper x = case x HM.! "name" of
       (String text) -> Just $ unpack text
       _ -> Nothing

nameToUUID' :: Manager -> TVar [(String, [String])] -> String -> IO (Maybe String)
nameToUUID' manager nameCache name = do
  cache <- atomically $ readTVar nameCache
  case filter ((name==) . head . snd) cache of
    [] -> nameToUUID manager name
    ((uuid, _) : _) -> return $ Just uuid

uuidToNames' :: Manager -> TVar [(String, [String])] -> String -> IO [String]
uuidToNames' manager nameCache uuid = do
  cache <- atomically $ readTVar nameCache
  case filter ((== uuid) . fst) cache of
    [] -> uuidToNames manager uuid
    ((_, names) : _) -> return names

dist :: Eq a => [a] -> [a] -> Int
dist a b =
  last
    ( if lab == 0
        then mainDiag
        else
          if lab > 0
            then lowers !! (lab - 1)
            else {- < 0 -} uppers !! (-1 - lab)
    )
  where
    mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
    uppers = eachDiag a b (mainDiag : uppers) -- upper diagonals
    lowers = eachDiag b a (mainDiag : lowers) -- lower diagonals
    eachDiag a [] diags = []
    eachDiag a (bch : bs) (lastDiag : diags) = oneDiag a bs nextDiag lastDiag : eachDiag a bs diags
      where
        nextDiag = head (tail diags)
    oneDiag a b diagAbove diagBelow = thisdiag
      where
        doDiag [] b nw n w = []
        doDiag a [] nw n w = []
        doDiag (ach : as) (bch : bs) nw n w = me : (doDiag as bs me (tail n) (tail w))
          where
            me = if ach == bch then nw else 1 + min3 (head w) nw (head n)
        firstelt = 1 + head diagBelow
        thisdiag = firstelt : doDiag a b firstelt diagAbove (tail diagBelow)
    lab = length a - length b
    min3 x y z = if x < y then x else min y z

data StatsResponse a
  = JustResponse a
  | NoResponse
  | DidYouMeanResponse a

maybeToJustStats :: Maybe a -> StatsResponse a
maybeToJustStats Nothing = NoResponse
maybeToJustStats (Just x) = JustResponse x
maybeToDidYouMeanStats :: Maybe a -> StatsResponse a
maybeToDidYouMeanStats Nothing = NoResponse
maybeToDidYouMeanStats (Just x) = DidYouMeanResponse x

flatNickList :: BowBotData -> IO [(String, String)]
flatNickList BowBotData {..} = do
  people <- atomically $ readTVar nickCache
  let currentNicks = [(n,u) | (n,us) <- people, u <- maybeToList (listToMaybe us)]
  let restOfNicks = [(n,u) | (n,us) <- people, u <- drop 1 us]
  return $ currentNicks ++ restOfNicks
  

searchForStats :: BowBotData -> Manager -> (String, String) -> IO (StatsResponse Stats)
searchForStats bbd manager (uuid, name) = do
  stats <- if null uuid then return Nothing else liftIO $ getStats manager uuid
  people <- flatNickList bbd
  case stats of
    Nothing -> do
      let dists = map (\(u,n) -> (u, dist (map toLower n) (map toLower name))) people
      let filtered = map fst . filter (\(u,d) -> d <= 2) $ dists
      case filtered of
        [] -> return NoResponse
        (nu:_) -> liftIO $ maybeToDidYouMeanStats <$> getStats manager nu
    s@(Just Stats {bowWins = 0, bowLosses = 0}) -> do
      let dists = map (\(u,n) -> (u, dist (map toLower n) (map toLower name))) people
      let filtered = map fst . filter (\(u,d) -> d <= 2) $ dists
      case filtered of
        [] -> return $ maybeToJustStats s
        (nu:_) -> liftIO $ maybeToDidYouMeanStats <$> getStats manager nu
    s -> return $ maybeToJustStats s

eventHandler :: BowBotData -> Event -> DiscordHandler ()
eventHandler dt@BowBotData {..} event = case event of
  MessageCreate m -> do
    unless (fromBot m) $ case unpack $ T.toLower . T.takeWhile (/= ' ') $ messageText m of
      "?s" -> do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        t <- liftIO $ read @Int <$> getTime "%S"
        cv <- liftIO . atomically $ do
          c1 <- readTVar count
          c2 <- readTVar countBorder
          let c = c1 + c2
          when (c < 15) $ modifyTVar (if t <= 5 || t >= 55 then countBorder else count) (+ 1)
          return $ c < 15
        if cv
          then do
            manager <- liftIO $ newManager tlsManagerSettings
            let name = unpack . strip . T.drop 2 $ messageText m
            uuid <- liftIO $ nameToUUID' manager nickCache name
            stats <- liftIO $ searchForStats dt manager (fromMaybe "" uuid, name)
            _ <- case stats of
              NoResponse -> restCall $ R.CreateMessage (messageChannel m) "*The player doesn't exist!*"
              (JustResponse s) -> restCall . R.CreateMessage (messageChannel m) . pack . showStats $ s
              (DidYouMeanResponse s) -> restCall . R.CreateMessage (messageChannel m) . ("*Did you mean* "<>) . pack . showStats $ s
            pure ()
          else do
            f <- liftIO $ read @Int <$> getTime "%S"
            _ <- restCall . R.CreateMessage (messageChannel m) . pack $ "**Too many requests! Wait another " ++ show ((65 - f) `mod` 60) ++ " seconds!**"
            pure ()
      "?online" -> do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        t <- liftIO $ read @Int <$> getTime "%S"
        cv <- liftIO . atomically $ do
          c <- readTVar onlineBusy
          unless c $ writeTVar onlineBusy True
          return (not c)
        if cv
          then do
            onlo <- liftIO . atomically $ readTVar online
            onlb <- liftIO . atomically $ readTVar online
            manager <- liftIO $ newManager tlsManagerSettings
            o <- case onlo <|> onlb of
              (Just onl) -> do
                _ <- restCall . R.CreateMessage (messageChannel m) $ "**Players in wachList currently in bow duels:** (cached response)"
                pure onl
              Nothing -> do
                people <- liftIO $ lines <$> readFile "watchlist.txt"
                status <- liftIO $ mapConcurrently (\u -> (u,) . fromMaybe False <$> isInBowDuels manager u) people
                let onl = map fst $ filter snd status
                liftIO . atomically $ writeTVar (if t <= 5 || t >= 55 then onlineBorder else online) $ Just onl
                _ <- restCall . R.CreateMessage (messageChannel m) $ "**Players in wachList currently in bow duels:**"
                pure onl
            liftIO . atomically $ writeTVar onlineBusy False
            names <- liftIO $ traverse (fmap last . uuidToNames' manager nickCache) o
            let msg = if null names then "None of the watchListed players are currently in bow duels." else pack . unlines . map (" - " ++) $ names
            _ <- restCall . R.CreateMessage (messageChannel m) $ "```" <> msg <> "```"
            pure ()
          else do
            _ <- restCall . R.CreateMessage (messageChannel m) . pack $ "**Processing list of online players. Please send command again later.**"
            pure ()
      "?list" -> do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        st <- liftIO $ lines <$> readFile "watchlist.txt"
        manager <- liftIO $ newManager tlsManagerSettings
        people <- traverse (liftIO . uuidToNames' manager nickCache) st
        let str = T.unwords . map pack . mapMaybe listToMaybe $ people
        _ <- restCall . R.CreateMessage (messageChannel m) $ "**Players in wachList:**" <> "```\n" <> str <> "```"
        pure ()
      "?help" -> do
        liftIO . putStrLn $ "recieved " ++ unpack (messageText m)
        _ <-
          restCall . R.CreateMessage (messageChannel m) $
            "**Bow bot help:**\n\n"
              <> "**Commands:**\n"
              <> " - **?help** - *display this message*\n"
              <> " - **?online** - *show all people from watchList currently in Bow Duels*\n"
              <> " - **?list** - *show all players in watchList*\n"
              <> " - **?s [name]** - *show player's Bow Duels stats*\n\n"
              <> "Made by **GregC**#9698"
        pure ()
      _ -> pure ()
  _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

command :: Applicative m => Message -> Text -> m () -> m ()
command m c = when (not (fromBot m) && isCommand c (messageText m))

isCommand :: Text -> Text -> Bool
isCommand c = (c `isPrefixOf`) . T.toLower
