module BowBot.Command.HashCommand where

import BowBot.Command
import System.Environment
import Data.Hashable
import Text.Printf (printf)
import qualified Data.ByteString as BS
import BowBot.Utils

calculateHash :: IO String
calculateHash = do
  path <- getExecutablePath
  content <- BS.readFile path
  let execHash = hash content
  return $ printf "%x" execHash

hashCommand :: Command
hashCommand = Command CommandInfo
  { commandName = "hash"
  , commandHelpEntries = [HelpEntry { helpUsage = "hash", helpDescription = "show the current hash of Bow Bot", helpGroup = "normal" }]
  , commandPerms = DefaultLevel
  , commandTimeout = 2
  } $ noArguments $ do
    hash <- liftIO calculateHash
    respond $ "hash: " <> pack hash