{-# LANGUAGE TypeFamilies #-}

module BowBot.Minecraft.Account where

import BowBot.Minecraft.Basic
import BowBot.DB.Typed
import BowBot.Network.Basic
import BowBot.Utils
import qualified Data.Text as T

data MinecraftAccount = MinecraftAccount
  { mcUUID :: !UUID
  , mcNames :: ![Text]
  } deriving (Show, Eq)

instance QueryParams MinecraftAccount where
  renderParams MinecraftAccount {..} = renderParams (mcUUID, T.intercalate "," mcNames) -- TODO: this is not good
instance QueryResults MinecraftAccount where
  convertResults fields strings = let
    (mcUUID, T.splitOn "," -> mcNames) = convertResults fields strings
      in MinecraftAccount {..}
instance QueryResultsSize MinecraftAccount where
  queryResultsSize _ = 2
instance DatabaseTable MinecraftAccount where
  type PrimaryKey MinecraftAccount = UUID
  databaseTableName _ = "minecraft"
  databaseColumnNames _ = ["uuid", "names"]
  databasePrimaryKey _ = "uuid"

getMinecraftAccountByUUID :: (MonadIOReader m r, Has Connection r) => UUID -> m (Maybe MinecraftAccount)
getMinecraftAccountByUUID uuid = queryOnlyLogT selectByPrimaryQuery (Only uuid)

storeMinecraftAccount :: (MonadIOReader m r, Has Connection r) => MinecraftAccount -> m ()
storeMinecraftAccount acc = void $ executeLogT insertQuery acc

updateMinecraftAccountCache :: (MonadIOReader m r, HasAll '[Manager, Connection] r) => Int -> m ()
updateMinecraftAccountCache index = do
  let helper MinecraftAccount {..} = do
        newName <- mojangUUIDToCurrentName mcUUID
        return MinecraftAccount {mcNames = if newName == listToMaybe mcNames then mcNames else maybeToList newName ++ mcNames , ..}
  cache <- queryLogT_ selectAllQuery
  let bigchunked = chunksOf 150 $ sortOn (uuidString . mcUUID) cache
  let chunk = if index >= length bigchunked then [] else bigchunked !! index
  updatedAccounts <- for chunk helper
  void $ executeManyLogT insertQuery $ filter (`notElem` cache) updatedAccounts

mcNameToUUID :: (MonadIOReader m r, HasAll '[Manager, Connection] r) => Text -> m (Maybe UUID)
mcNameToUUID name = do
  goodAcc <- getMinecraftAccountByCurrentName name
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

getMinecraftAccountByCurrentName :: (MonadIOReader m r, Has Connection r) => Text -> m (Maybe MinecraftAccount)
getMinecraftAccountByCurrentName name = queryOnlyLogT (selectQueryWithSuffix " WHERE substring_index(`names`,',',1) = ?") (Only name)

addMinecraftName :: (MonadIOReader m r, Has Connection r) => Text -> UUID -> m Bool
addMinecraftName name uuid = addMinecraftNames [(name, uuid)]

addMinecraftNames :: (MonadIOReader m r, Has Connection r) => [(Text, UUID)] -> m Bool
addMinecraftNames namePairs = (>0) <$> executeManyLog "INSERT INTO `minecraft_name` (`name`, `minecraft_uuid`) VALUES (?,?) ON DUPLICATE KEY UPDATE `minecraft_uuid`=VALUES(`minecraft_uuid`)" namePairs

data MinecraftAutocorrect = MinecraftAutocorrect 
  { autocorrectAccount :: MinecraftAccount
  , autocorrectIsDirect :: Bool
  , autocorrectPastName :: Maybe Text
  }

autocorrectFromAccountDirect :: MinecraftAccount -> MinecraftAutocorrect
autocorrectFromAccountDirect acc = MinecraftAutocorrect { autocorrectAccount = acc, autocorrectIsDirect = True, autocorrectPastName = Nothing }

minecraftAutocorrect :: (MonadIOReader m r, Has Connection r) => Text -> m (Maybe MinecraftAutocorrect)
minecraftAutocorrect name = do
  people <- queryLogT_ selectAllQuery -- TODO: start using minecraftName table
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
