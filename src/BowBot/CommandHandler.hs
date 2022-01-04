module BowBot.CommandHandler(
  module BowBot.CommandHandler, module BowBot.CommandMonads
) where

import Discord
import BowBot.Utils
import qualified Discord.Requests as R
import qualified Discord.Internal.Rest as R
import Discord.Types hiding (accountId)
import BowBot.BotData
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Control.Exception.Base (evaluate)
import Control.DeepSeq (force, NFData(..))
import BowBot.DiscordNFData()
import Control.Monad (ap, liftM)
import BowBot.CommandMonads
import Control.Concurrent.STM.TVar (TVar)
import Network.HTTP.Conduit (Manager)

newtype CommandHandler a = CommandHandler { runCommandHandler :: Message -> Manager -> BotData -> DiscordHandler a }

instance Functor CommandHandler where
  fmap = liftM

instance Applicative CommandHandler where
  pure = return
  (<*>) = ap

instance Monad CommandHandler where
  return x = CommandHandler $ \_ _ _ -> pure x
  (CommandHandler f) >>= g = CommandHandler $ \a b c -> do
    v <- f a b c
    runCommandHandler (g v) a b c

instance MonadIO CommandHandler where
  liftIO m = CommandHandler $ \_ _ _ -> liftIO m

instance APIMonad CommandHandler where
  hManager = CommandHandler $ \_ man _ -> return man

instance BotDataMonad CommandHandler where
  hData = CommandHandler $ \_ _ dt -> return dt
  
instance DiscordMonad CommandHandler where
  hDiscord d = CommandHandler $ \_ _ _ -> d

hRespond :: String -> CommandHandler ()
hRespond msg = CommandHandler $ \m _ _ -> respond m msg

hRespondFile :: T.Text -> String -> CommandHandler ()
hRespondFile n s = CommandHandler $ \m _ _ -> respondFile m n s

hCaller :: CommandHandler User
hCaller = CommandHandler $ \m _ _ -> return $ messageAuthor m

hArg :: Int -> CommandHandler (Maybe String)
hArg n = CommandHandler $ \m _ _ -> pure $ let args = words (unpack (messageText m)) in if length args > n then Just (args !! n) else Nothing

hArgs :: CommandHandler [String]
hArgs = CommandHandler $ \m _ _ -> pure $ tail $ words (unpack (messageText m))

call :: (FromJSON a, R.Request (r a), NFData (r a)) => r a -> DiscordHandler (Either RestCallErrorCode a)
call r = liftIO (evaluate (force r)) >>= restCall

call_ :: (FromJSON a, R.Request (r a), NFData (r a)) => r a -> DiscordHandler ()
call_ r = void $ call r

respond :: Message -> String -> DiscordHandler ()
respond m s = call_ $ R.CreateMessage (messageChannel m) $ pack s

respondFile :: Message -> T.Text -> String -> DiscordHandler ()
respondFile m n s = call_ $ R.CreateMessageUploadFile (messageChannel m) n $ encodeUtf8 $ pack s