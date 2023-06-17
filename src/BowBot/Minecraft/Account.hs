{-# LANGUAGE TypeFamilies #-}

module BowBot.Minecraft.Account where

import BowBot.Minecraft.Basic
import BowBot.BotData.Cached
import BowBot.DB.Basic
import BowBot.Network.Basic
import BowBot.Utils
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

data MinecraftAccount = MinecraftAccount
  { mcUUID :: !UUID
  , mcNames :: ![Text]
  } deriving (Show, Eq)

instance Cached MinecraftAccount where
  type CacheIndex MinecraftAccount = UUID
  refreshCache = do
    cache <- getCache
    res :: [(UUID, Text)] <- queryLog "SELECT `uuid`, `names` FROM `minecraft`" ()
    let newValues = HM.fromList $ flip fmap res $ \(mcUUID, T.splitOn "," -> mcNames) -> (mcUUID, MinecraftAccount {..})
    liftIO $ atomically $ writeTVar cache newValues

instance CachedIndexed MinecraftAccount where
  cacheIndex = mcUUID
  storeInCache accs = do
    cacheMap <- getCacheMap
    let toQueryParams acc@MinecraftAccount {..} = if Just acc == cacheMap HM.!? mcUUID then Nothing else Just (uuidString mcUUID, head mcNames, T.intercalate "," mcNames)
    let queryParams = mapMaybe toQueryParams accs
    success <- liftIO $ withDB $ \conn -> (>0) <$> executeManyLog' conn "INSERT INTO `minecraft` (`uuid`, `name`, `names`) VALUES (?,?,?) ON DUPLICATE KEY UPDATE `name`=VALUES(`name`), `names`=VALUES(`names`), `hypixel`=VALUES(`hypixel`)" queryParams
    when success $ do
      cache <- getCache
      liftIO $ atomically $ modifyTVar cache (insertMany (map (\x -> (mcUUID x, x)) accs))
    return success

updateMinecraftAccountCache :: (MonadIOBotData m d r, Has Manager r, HasCache MinecraftAccount d) => Int -> m ()
updateMinecraftAccountCache index = do
  let helper MinecraftAccount {..} = do
        newName <- mojangUUIDToCurrentName mcUUID
        return MinecraftAccount {mcNames = if newName == listToMaybe mcNames then mcNames else maybeToList newName ++ mcNames , ..}
  cache <- HM.elems <$> getCacheMap
  let bigchunked = chunksOf 150 $ sortOn (uuidString . mcUUID) cache
  let chunk = if index >= length bigchunked then [] else bigchunked !! index
  updatedAccounts <- for chunk helper
  void $ storeInCache updatedAccounts

mcNameToUUID :: (MonadIOBotData m d r, Has Manager r, HasCache MinecraftAccount d) => Text -> m (Maybe UUID)
mcNameToUUID name = do
  goodAcc <- getMinecraftAccountByCurrentNameFromCache name
  case goodAcc of
    Just MinecraftAccount {mcUUID} -> return (Just mcUUID)
    _ -> mojangNameToUUID name

freshMinecraftAccount :: UUID -> Text -> MinecraftAccount
freshMinecraftAccount mcUUID name = MinecraftAccount { mcUUID, mcNames = [name, name <> "OldNamesCurrentlyNotKnown"] }

freshMinecraftAccountByUUID :: (MonadIOReader m r, Has Manager r) => UUID -> m (Maybe MinecraftAccount)
freshMinecraftAccountByUUID uuid = do
  name <- mojangUUIDToCurrentName uuid
  return $ freshMinecraftAccount uuid <$> name

freshMinecraftAccountByName :: (MonadIOReader m r, Has Manager r) => Text -> m (Maybe MinecraftAccount)
freshMinecraftAccountByName name = do
  uuid <- mojangNameToUUID name
  join <$> for uuid freshMinecraftAccountByUUID

getMinecraftAccountByCurrentNameFromCache :: (MonadIOBotData m d r, HasCache MinecraftAccount d) => Text -> m (Maybe MinecraftAccount)
getMinecraftAccountByCurrentNameFromCache name = find ((==T.toLower name) . T.toLower . head . mcNames) . HM.elems <$> getCacheMap

addMinecraftName :: (MonadIOReader m r, Has Connection r) => Text -> UUID -> m Bool
addMinecraftName name uuid = addMinecraftNames [(name, uuid)]

addMinecraftNames :: (MonadIOReader m r, Has Connection r) => [(Text, UUID)] -> m Bool
addMinecraftNames namePairs = (>0) <$> executeManyLog "INSERT INTO `minecraftName` (`name`, `uuid`) VALUES (?,?) ON DUPLICATE KEY UPDATE `name`=VALUES(`name`), `uuid`=VALUES(`uuid`)" namePairs

data MinecraftAutocorrect = MinecraftAutocorrect 
  { autocorrectAccount :: MinecraftAccount
  , autocorrectIsDirect :: Bool
  , autocorrectPastName :: Maybe Text
  }

autocorrectFromAccountDirect :: MinecraftAccount -> MinecraftAutocorrect
autocorrectFromAccountDirect acc = MinecraftAutocorrect { autocorrectAccount = acc, autocorrectIsDirect = True, autocorrectPastName = Nothing }

minecraftAutocorrect :: (MonadIOBotData m d r, HasCache MinecraftAccount d) => Text -> m (Maybe MinecraftAutocorrect)
minecraftAutocorrect name = do
  people <- HM.elems <$> getCacheMap
  let process isPast = map snd . sortOn fst $ do
        acc@MinecraftAccount {..} <- people
        n <- (if isPast then drop 1 else take 1) mcNames
        let d = dist (T.toLower n) (T.toLower name)
        guard (d <= 2)
        return (d, MinecraftAutocorrect { autocorrectAccount = acc, autocorrectIsDirect = d == 0, autocorrectPastName = if isPast then Just n else Nothing })
  return $ listToMaybe $ process False ++ process True

minecraftAccountToHeader :: MinecraftAccount -> Maybe Text -> Text
minecraftAccountToHeader MinecraftAccount {..} Nothing = "**" <> discordEscape (head mcNames) <> "**:\n"
minecraftAccountToHeader MinecraftAccount {..} (Just name) = "**" <> discordEscape name <> "** (" <> discordEscape (head mcNames) <> "):\n"

minecraftAutocorrectToHeader :: MinecraftAutocorrect -> Text
minecraftAutocorrectToHeader MinecraftAutocorrect {..} =
  (if autocorrectIsDirect then "" else "*Did you mean* ") <> minecraftAccountToHeader autocorrectAccount autocorrectPastName
