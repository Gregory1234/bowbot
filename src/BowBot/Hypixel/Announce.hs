module BowBot.Hypixel.Announce where

import qualified Discord.Requests as R
import BowBot.Account.Basic
import qualified Data.HashMap.Strict as HM
import BowBot.Discord.Utils
import BowBot.BotData.Info
import BowBot.Discord.Account
import BowBot.Minecraft.Basic
import BowBot.Minecraft.Account
import Data.Bifunctor (first)
import qualified Data.Text as T
import BowBot.DB.Basic

milestonesChannelInfo :: InfoType ChannelId
milestonesChannelInfo = InfoType { infoName = "milestones_channel", infoDefault = 0, infoParse = first pack . readEither . unpack }

milestoneNamesInfo :: InfoType [(Integer, Text)]
milestoneNamesInfo = InfoType { infoName = "division_title_milestones", infoDefault = [], infoParse = \s -> for (T.lines s) $ \l -> case T.splitOn "->" l of [a, b] -> (,b) <$> fmap fromInteger ((first pack . readEither . unpack) a); _ -> Left "wrong format" }

milestoneNamesFromWins :: [(Integer, Text)] -> Integer -> Integer -> [Text]
milestoneNamesFromWins names low high = map snd $ filter (\(needed, _) -> low < needed && needed <= high) names

getHypixelBowMilestones :: (MonadIOReader m r, HasAll [InfoCache, Connection] r) => m [(UUID, Text)]
getHypixelBowMilestones = do
  ctx <- ask
  milestoneNames <- askInfo milestoneNamesInfo
  milestonePairs <- liftIO $ (`runReaderT` ctx) $ withTransaction $ do
    res :: [(UUID, Integer, Integer)] <- queryLog "SELECT `minecraft`, `announcementWins`, `bowWins` FROM `stats` WHERE `bowWins` > `announcementWins` AND `announcementWins` != -1" ()
    void $ executeLog "UPDATE `stats` SET `announcementWins`=`bowWins`" ()
    return res
  return [(uuid, milestone) | (uuid, low, high) <- milestonePairs, milestone <- milestoneNamesFromWins milestoneNames low high]

announceMilestones :: (MonadIOReader m r, HasAll [DiscordHandle, Connection, InfoCache] r) => m ()
announceMilestones = do
  milestonesChannel <- askInfo milestonesChannelInfo
  toAnnounce <- getHypixelBowMilestones
  dcMembers :: HM.HashMap UserId DiscordAccount <- HM.fromList . map (\x -> (discordId x, x)) <$> getDiscordGuildMemberAccounts
  for_ toAnnounce $ \(uuid, milestone) -> do
    accountDiscords <- getDiscordIdsByMinecraft uuid
    mcacc <- fromJust <$> getMinecraftAccountByUUID uuid
    let p = map (\x -> x { discordName = (discordName x) { discordNickname = Nothing } }) $ mapMaybe (dcMembers HM.!?) accountDiscords
    unless (null p) $ do
      call_ $ R.CreateMessage milestonesChannel $ "**Congratulations** to **" <> head (mcNames mcacc) <> "** (" <> T.intercalate ", " (map (showDiscordNameDiscord . discordName) p) <> ") for reaching " <> milestone <> "!"