{-# LANGUAGE QuasiQuotes #-}

module BowBot.Network.ClearLogs where

import BowBot.Utils
import Network.Mail.Mime hiding (simpleMail)
import BowBot.DB.Basic
import Data.Time
import qualified Data.Text as T
import Codec.Compression.GZip
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

clearLogs :: (MonadIOReader m r, Has Connection r) => m ()
clearLogs = do -- TODO: timezones?
  logs :: [(UTCTime, Text, Text)] <- queryLog [mysql|SELECT `timestamp`,`message`,`type` FROM `logs`|]
  let showLogLine (time, msg, typ) = "[" <> typ <> ", " <> pack (formatTime defaultTimeLocale "%d %b %Y %H:%M:%S" time) <> "]: " <> msg
  let logsFile = T.unlines $ map showLogLine logs
  let zippedLogsFile = compress $ BS.fromStrict $ T.encodeUtf8 logsFile
  liftIO $ do
    mailFrom <- getEnvOrThrow "MAIL_FROM"
    mailTo <- getEnvOrThrow "MAIL_TO"
    let from = Address (Just "BowBot") (pack mailFrom)
    let to = Address Nothing (pack mailTo)
    date <- getTime "%d %b %Y %H:%M"
    let subject = "Bowbot logs " <> pack date
    renderSendMailCustom "/usr/sbin/sendmail" ["-t", "-i"] 
        $ addAttachmentBS "application/octet-stream" "logs.gz" zippedLogsFile 
        $ simpleMail' to from subject "..."
  void $ executeLog [mysql|DELETE FROM `logs`|]