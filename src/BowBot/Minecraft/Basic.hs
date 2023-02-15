module BowBot.Minecraft.Basic where

import BowBot.Network.Basic hiding (Result)
import Data.Hashable (Hashable)
import BowBot.Utils
import qualified Data.Text as T
import Database.MySQL.Simple (Param, Result)

newtype UUID = UUID { uuidString :: Text }
  deriving (Show, Eq, Ord)
  deriving newtype (Hashable, Param, Result)

uuidFromString :: Text -> Maybe UUID
uuidFromString str
  | T.length str == 36
  , T.index str 8 == '-'
  , T.index str 13 == '-'
  , T.index str 18 == '-'
  , T.index str 32 == '-'
  = uuidFromString $ T.filter (/='-') str
  | T.length str == 32
  , T.all (`elem` ("1234567890abcdefABCDEF" :: String)) str
  = Just . UUID $ T.map toLower str
  | otherwise = Nothing

mojangNameToUUID :: (MonadIOReader m r, Has Manager r) => Text -> m (Maybe UUID)
mojangNameToUUID name = do
  let url = "https://api.mojang.com/users/profiles/minecraft/" <> name
  res <- sendRequestTo url url
  decodeParse res $ \o -> UUID <$> o .: "id"

mojangUUIDToCurrentName :: (MonadIOReader m r, Has Manager r) => UUID -> m (Maybe Text)
mojangUUIDToCurrentName (UUID uuid) = do
  let url = "https://sessionserver.mojang.com/session/minecraft/profile/" <> uuid
  res <- sendRequestTo url url
  decodeParse res $ \o -> o .: "name"