{-# LANGUAGE QuasiQuotes #-}

module BowBot.Ranked.ModCommands where

import BowBot.Command
import BowBot.DB.Basic
import BowBot.Ranked.Queue
import BowBot.Discord.Utils
import Control.Monad.Except (ExceptT)
import BowBot.BotData.Info
import Control.Monad.Error.Class (throwError)
import BowBot.Ranked.Game
import BowBot.Minecraft.Basic
import BowBot.Minecraft.Account
import BowBot.Command.Utils
import BowBot.Account.Basic
import BowBot.Ranked.EloUpdate

-- TODO: remake it using the permission system
restrictToRankedMods :: ExceptT Text CommandHandler () -> ExceptT Text CommandHandler ()
restrictToRankedMods body = do
  did <- userId <$> envs envSender
  modRole <- askInfo rankedModRoleInfo
  guildId <- askInfo discordGuildIdInfo
  guildMember <- liftMaybe somethingWentWrongMessage =<< getDiscordGuildMember guildId did
  if modRole `elem` memberRoles guildMember
    then body
    else respond "*You are not a ranked mod!*"

delQueueCommand :: Command
delQueueCommand = Command CommandInfo
  { commandName = "delqueue"
  , commandHelpEntries = [HelpEntry { helpUsage = "delqueue", helpDescription = "clear all Ranked Bow queues", helpGroup = "rankedmod" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ noArguments $ restrictToRankedMods $ do
    void $ executeLog [mysql|UPDATE `ranked_bow_stats` SET `in_queue` = 0|]
    respond "*Cleared all ranked queues!*"

abandonGameCommand :: Command
abandonGameCommand = Command CommandInfo
  { commandName = "abandongame"
  , commandHelpEntries = 
    [ HelpEntry { helpUsage = "abandongame [number]", helpDescription = "abandon a game", helpGroup = "rankedmod" }
    , HelpEntry { helpUsage = "abandongame [number] [player to punish] [punishment]", helpDescription = "abandon a game and give a player an elo punishment", helpGroup = "rankedmod" } ]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ oneOrThreeArguments $ \gameIdStr punishment -> restrictToRankedMods $ do
    gameId <- liftMaybe "*Wrong number!*" (readMaybe $ unpack gameIdStr)
    game <- liftMaybe "*Game not found!*" =<< getRankedGameById gameId
    unless (rankedGameStatus game == GameActive) $ throwError "*That game is already completed!*"
    eloChanges <- case punishment of
      Nothing -> return []
      Just (player, readMaybe . unpack -> Just (negate . abs -> elo)) -> do
        people <- queryLog [mysql|SELECT MinecraftAccount FROM `ranked_bow` JOIN `minecraft` ON `uuid` = `ranked_uuid`|]
        ac <- liftMaybe thePlayerDoesNotExistMessage $ minecraftAutocorrectGeneral people player
        let uuid = mcUUID $ autocorrectAccount ac
        bid <- liftMaybe somethingWentWrongMessage =<< queryOnlyLog [mysql|SELECT `account_id` FROM `ranked_bow` WHERE `ranked_uuid` = uuid|]
        when (bid /= fst (rankedPlayers game) && bid /= snd (rankedPlayers game)) $ throwError "*The punished player is not a part of that game!*"
        return [(bid, elo)]
      _ -> throwError "*Wrong elo!*"
    c1 <- finalizeRankedGame GameAbandoned gameId
    unless c1 $ throwError somethingWentWrongMessage
    eloChanges' <- liftMaybe somethingWentWrongMessage =<< applyPureEloUpdate (rankedGameQueue game) eloChanges
    c3 <- announceEloUpdate (rankedGameQueue game) (Just (False, rankedGameId game)) eloChanges'
    unless c3 $ throwError somethingWentWrongMessage
    respond "*Ranked game abandoned!*"
    
changeEloCommand :: Command
changeEloCommand = Command CommandInfo
  { commandName = "changeelo"
  , commandHelpEntries = [ HelpEntry { helpUsage = "changeelo [queue] [player] [change]", helpDescription = "change a player's elo", helpGroup = "rankedmod" } ]
  , commandPerms = DefaultLevel
  , commandTimeout = 15
  } $ threeArguments $ \qName player eloChangeStr -> restrictToRankedMods $ do
    queue <- liftMaybe "*Bad queue name!*" =<< getQueueByName qName
    eloChange <- case unpack eloChangeStr of
      ('+':(readMaybe -> Just elo)) -> return elo
      ('-':(readMaybe -> Just elo)) -> return $ -elo
      _ -> throwError "*Wrong elo!*"
    people <- queryLog [mysql|SELECT MinecraftAccount FROM `ranked_bow` JOIN `minecraft` ON `uuid` = `ranked_uuid`|]
    ac <- liftMaybe thePlayerDoesNotExistMessage $ minecraftAutocorrectGeneral people player
    let uuid = mcUUID $ autocorrectAccount ac
    bid <- liftMaybe somethingWentWrongMessage =<< queryOnlyLog [mysql|SELECT `account_id` FROM `ranked_bow` WHERE `ranked_uuid` = uuid|]
    let eloChanges = [(bid, eloChange)]
    eloChanges' <- liftMaybe somethingWentWrongMessage =<< applyPureEloUpdate queue eloChanges
    c3 <- announceEloUpdate queue Nothing eloChanges'
    unless c3 $ throwError somethingWentWrongMessage
    respond "*Elo changed!*"