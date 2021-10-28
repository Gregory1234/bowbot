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
  , (25000, 770446876411035669)
  , (50000, 903372820237656196)
  ]

testDiscordDivisionRoles :: [(Integer, RoleId)]
testDiscordDivisionRoles = [(15000, 900080311822581771)]

airplanesMemberVisitorRoles :: (RoleId, RoleId)
airplanesMemberVisitorRoles = (742890312690827345, 742874367200854136)

testDiscordMemberVisitorRoles :: (RoleId, RoleId)
testDiscordMemberVisitorRoles = (900346056968130640, 900346092850401310)

testDiscordIllegalRole :: RoleId
testDiscordIllegalRole = 901109205145096232

airplanesIllegalRole :: RoleId
airplanesIllegalRole = 901117803023728700

airplanesHypixelId :: String
airplanesHypixelId = "5f308a358ea8c97248581e46"