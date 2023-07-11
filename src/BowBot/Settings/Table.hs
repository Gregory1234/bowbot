{-# LANGUAGE TypeFamilies #-}

module BowBot.Settings.Table where

import BowBot.Discord.Utils
import BowBot.DB.Typed
import BowBot.Settings.Single

data SettingsTable a where
  SettingsTDiscord :: SettingsTable UserId
  SettingsTWins :: SettingsTable SettingBin
  SettingsTLosses :: SettingsTable SettingBin
  SettingsTWLR :: SettingsTable SettingTer
  SettingsTWinsUntil :: SettingsTable SettingTer
  SettingsTBestStreak :: SettingsTable SettingTer
  SettingsTCurrentStreak :: SettingsTable SettingTer
  SettingsTBestDailyStreak :: SettingsTable SettingTer
  SettingsTBowHits :: SettingsTable SettingBin
  SettingsTBowShots :: SettingsTable SettingBin
  SettingsTAccuracy :: SettingsTable SettingTer

instance DatabaseTableLike SettingsTable where
  columnName SettingsTDiscord = "discord_id"
  columnName SettingsTWins = "wins"
  columnName SettingsTLosses = "losses"
  columnName SettingsTWLR = "wlr"
  columnName SettingsTWinsUntil = "wins_until"
  columnName SettingsTBestStreak = "best_streak"
  columnName SettingsTCurrentStreak = "current_streak"
  columnName SettingsTBestDailyStreak = "best_daily_streak"
  columnName SettingsTBowHits = "bow_hits"
  columnName SettingsTBowShots = "bow_shots"
  columnName SettingsTAccuracy = "accuracy"

instance DatabaseTable SettingsTable where
  newtype PrimaryKey SettingsTable = SettingsTPrimary { getSettingsTPrimary :: UserId }
    deriving newtype (QueryParams, QueryResults)
  tableName = "settings"

instance InTable SettingsTable (PrimaryKey SettingsTable) where
  columnRep = ColRep [SomeCol SettingsTDiscord]

type instance MainTable (PrimaryKey SettingsTable) = SettingsTable