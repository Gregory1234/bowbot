module BowBot.Constants where

import Discord.Types


airplanesId :: GuildId
airplanesId = 742731987902791751

testDiscordId :: GuildId
testDiscordId = 839516111448178729

airplanesDivisionRoles :: [(Integer, RoleId)]
airplanesDivisionRoles =
  [ (100, 884063678360596480)
  , (250, 884063422155730984)
  , (500, 865997053838753814)
  , (1000, 742734559514329239)
  , (2000, 742734346200285277)
  , (5000, 742734209084555264)
  , (10000, 742734125231898656)
  , (28000, 770446876411035669)
  ]

testDiscordDivisionRoles :: [(Integer, RoleId)]
testDiscordDivisionRoles = [(15000, 900080311822581771)]