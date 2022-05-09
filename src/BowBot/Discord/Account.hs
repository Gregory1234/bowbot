{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module BowBot.Discord.Account where

import BowBot.BotData.Cached
import BowBot.Discord.Utils
import qualified Data.HashMap.Strict as HM
import BowBot.DB.Basic
import Data.Maybe (mapMaybe)
import BowBot.BotData.Info
import qualified Discord.Requests as R
import Data.List (deleteFirstsBy)

data DiscordAccount = DiscordAccount { discordId :: UserId, discordName :: String, discordDiscrim :: String, discordNickname :: Maybe String } deriving (Show, Eq)

instance Cached DiscordAccount where
  type CacheIndex DiscordAccount = UserId
  refreshCache conn = do
    cache <- getCache
    res :: [(Integer, String, String, Maybe String)] <- queryLog conn "SELECT `id`, `name`, `discriminator`, `nickname` FROM `discordDEV`" ()
    let newValues = HM.fromList $ flip fmap res $ \(fromIntegral -> discordId, discordName, discordDiscrim, discordNickname) -> (discordId, DiscordAccount {..})
    liftIO $ atomically $ writeTVar cache newValues

instance CachedIndexed DiscordAccount where
  cacheIndex = discordId
  storeInCache accs = do
    cacheMap <- getCacheMap
    let toQueryParams acc@DiscordAccount {..} = if Just acc == cacheMap HM.!? discordId then Nothing else Just (toInteger discordId, discordName, discordDiscrim, discordNickname)
    let queryParams = mapMaybe toQueryParams accs
    success <- liftIO $ withDB $ \conn -> (>0) <$> executeManyLog conn "INSERT INTO `discordDEV` (`id`, `name`, `discriminator`, `nickname`) VALUES (?,?,?,?) ON DUPLICATE KEY UPDATE `name`=VALUES(`name`), `discriminator`=VALUES(`discriminator`), `nickname`=VALUES(`nickname`)" queryParams
    when success $ do
      cache <- getCache
      liftIO $ atomically $ modifyTVar cache (insertMany (map (\x -> (discordId x, x)) accs))
    return success

guildMemberToDiscordAccount :: GuildMember -> DiscordAccount
guildMemberToDiscordAccount GuildMember { memberUser = Just user, .. } = (userToDiscordAccount user) { discordNickname = unpack <$> memberNick }
guildMemberToDiscordAccount _ = DiscordAccount 0 "" "0000" Nothing

userToDiscordAccount :: User -> DiscordAccount
userToDiscordAccount User {..} = DiscordAccount
  { discordId = userId
  , discordName = unpack userName
  , discordDiscrim = maybe "0000" unpack userDiscrim
  , discordNickname = Nothing
  }

showDiscordAccount :: DiscordAccount -> String
showDiscordAccount DiscordAccount { discordNickname = Nothing, ..} = discordName ++ "#" ++ discordDiscrim
showDiscordAccount DiscordAccount { discordNickname = Just nick, ..} = nick ++ " (" ++ discordName ++ "#" ++ discordDiscrim ++ ")"

showDiscordAccountDiscord :: DiscordAccount -> String
showDiscordAccountDiscord DiscordAccount { discordNickname = Nothing, ..} = "**" ++ discordEscape discordName ++ "**#" ++ discordDiscrim
showDiscordAccountDiscord DiscordAccount { discordNickname = Just nick, ..} = "**" ++ discordEscape nick ++ "** (" ++ discordEscape discordName ++ "#" ++ discordDiscrim ++ ")"

updateDiscordAccountCache :: (MonadDiscord m, MonadCache InfoField m, MonadCache DiscordAccount m) => m ()
updateDiscordAccountCache = do
  gid <- hInfoDB discordGuildIdInfo
  members <- map guildMemberToDiscordAccount . filter (\GuildMember {..} -> fmap userIsBot memberUser == Just False) <$> discordGuildMembers gid
  current <- HM.elems <$> getCacheMap
  updatedNonMembers <- for (deleteFirstsBy (\a b -> discordId a == discordId b) current members) $ \du -> do
    u' <- call $ R.GetUser (discordId du)
    case u' of
      Left e -> do
        logError $ show e
        return du
      Right u -> return $ userToDiscordAccount u
  void $ storeInCache $ members ++ updatedNonMembers