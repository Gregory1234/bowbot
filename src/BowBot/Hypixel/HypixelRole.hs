module BowBot.Hypixel.HypixelRole where

import BowBot.DB.Typed
import Data.Text (Text)

newtype HypixelRole = HypixelRole { fromHypixelRole :: Text }
  deriving (Eq, Ord, Show)
  deriving newtype (Param, Result, QueryParams, QueryResults)