{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BowBot.Minecraft.Account where

import BowBot.Minecraft.Basic
import BowBot.DB.Basic
import BowBot.Network.Basic (Manager)
import BowBot.Utils
import qualified Data.Text as T

instance ToMysqlSimple [Text] where
  toMysqlValue = toMysqlValue . T.intercalate ","

instance FromMysqlSimple [Text] where
  fromMysqlValue = T.splitOn "," . fromMysqlValue

deriving via (SimpleValue [Text]) instance ToMysql [Text]
deriving via (SimpleValue [Text]) instance FromMysql [Text]

data MinecraftAccount = MinecraftAccount
  { mcUUID :: !UUID
  , mcNames :: ![Text]
  } deriving stock (Show, Eq, Generic)
    deriving (ToMysql, FromMysql) via (Generically MinecraftAccount)

$(pure [])

getMinecraftAccountByUUID :: (MonadIOReader m r, Has SafeMysqlConn r) => UUID -> m (Maybe MinecraftAccount)
getMinecraftAccountByUUID uuid = queryOnlyLog [mysql|SELECT MinecraftAccount FROM `minecraft` WHERE `uuid` = uuid|]

storeMinecraftAccount :: (MonadIOReader m r, Has SafeMysqlConn r) => MinecraftAccount -> m ()
storeMinecraftAccount acc = void $ executeLog [mysql|INSERT INTO `minecraft`(MinecraftAccount) VALUES acc|]

updateMinecraftAccountCache :: (MonadIOReader m r, HasAll '[Manager, SafeMysqlConn] r) => Int -> m ()
updateMinecraftAccountCache index = do
  let helper MinecraftAccount {..} = do
        newName <- mojangUUIDToCurrentName mcUUID
        return MinecraftAccount {mcNames = if newName == listToMaybe mcNames then mcNames else maybeToList newName ++ mcNames , ..}
  cache <- queryLog [mysql|SELECT MinecraftAccount FROM `minecraft`|]
  let bigchunked = chunksOf 150 $ sortOn (uuidString . mcUUID) cache
  let chunk = if index >= length bigchunked then [] else bigchunked !! index
  updatedAccounts <- for chunk helper
  let toInsert = filter (`notElem` cache) updatedAccounts
  void $ executeLog [mysql|INSERT INTO `minecraft`(MinecraftAccount) VALUES toInsert..|]

mcNameToUUID :: (MonadIOReader m r, HasAll '[Manager, SafeMysqlConn] r) => Text -> m (Maybe UUID)
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

getMinecraftAccountByCurrentName :: (MonadIOReader m r, Has SafeMysqlConn r) => Text -> m (Maybe MinecraftAccount)
getMinecraftAccountByCurrentName name = queryOnlyLog [mysql|SELECT MinecraftAccount FROM `minecraft` WHERE SUBSTRING_INDEX(`names`,",",1) = name|]

addMinecraftName :: (MonadIOReader m r, Has SafeMysqlConn r) => Text -> UUID -> m Bool
addMinecraftName name uuid = addMinecraftNames [(name, uuid)]

addMinecraftNames :: (MonadIOReader m r, Has SafeMysqlConn r) => [(Text, UUID)] -> m Bool
addMinecraftNames namePairs = (>0) <$> executeLog [mysql|INSERT INTO `minecraft_name` (`name`, ^`minecraft_uuid`) VALUES namePairs..|]

data MinecraftAutocorrect = MinecraftAutocorrect 
  { autocorrectAccount :: MinecraftAccount
  , autocorrectIsDirect :: Bool
  , autocorrectPastName :: Maybe Text
  }

autocorrectFromAccountDirect :: MinecraftAccount -> MinecraftAutocorrect
autocorrectFromAccountDirect acc = MinecraftAutocorrect { autocorrectAccount = acc, autocorrectIsDirect = True, autocorrectPastName = Nothing }

minecraftAutocorrectGeneral :: [MinecraftAccount] -> Text -> Maybe MinecraftAutocorrect
minecraftAutocorrectGeneral people name = listToMaybe $ process False ++ process True
  where
    process isPast = map snd . sortOn fst $ do
      acc@MinecraftAccount {..} <- people
      n <- (if isPast then drop 1 else take 1) mcNames
      let d = dist (T.toLower n) (T.toLower name)
      guard (d <= 2)
      return (d, MinecraftAutocorrect { autocorrectAccount = acc, autocorrectIsDirect = d == 0, autocorrectPastName = if isPast then Just n else Nothing })

minecraftAutocorrect :: (MonadIOReader m r, Has SafeMysqlConn r) => Text -> m (Maybe MinecraftAutocorrect)
minecraftAutocorrect name = minecraftAutocorrectGeneral <$> queryLog [mysql|SELECT MinecraftAccount FROM `minecraft`|] <*> pure name -- TODO: start using minecraftName table

minecraftAccountToHeader :: MinecraftAccount -> Maybe Text -> Text
minecraftAccountToHeader MinecraftAccount {..} Nothing = "**" <> discordEscape (head mcNames) <> "**:\n"
minecraftAccountToHeader MinecraftAccount {..} (Just name) = "**" <> discordEscape name <> "** (" <> discordEscape (head mcNames) <> "):\n"

minecraftAutocorrectToHeader :: MinecraftAutocorrect -> Text
minecraftAutocorrectToHeader MinecraftAutocorrect {..} =
  (if autocorrectIsDirect then "" else "*Did you mean* ") <> minecraftAccountToHeader autocorrectAccount autocorrectPastName
