module BowBot.CommandHandler(
  module BowBot.CommandHandler, module BowBot.API
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
import BowBot.API
import Control.Concurrent.STM.TVar (TVar)

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

hRespond :: String -> CommandHandler ()
hRespond msg = CommandHandler $ \m _ _ -> respond m msg

hRespondFile :: T.Text -> String -> CommandHandler ()
hRespondFile n s = CommandHandler $ \m _ _ -> respondFile m n s

hCaller :: CommandHandler User
hCaller = CommandHandler $ \m _ _ -> return $ messageAuthor m

hRead :: (BotData -> TVar a) -> CommandHandler a
hRead p = CommandHandler $ \_ _ dt -> readProp p dt

hModify :: (BotData -> TVar a) -> (a -> a) -> CommandHandler ()
hModify p f = CommandHandler $ \_ _ dt -> modifyProp p dt f

hWrite :: (BotData -> TVar a) -> a -> CommandHandler ()
hWrite p v = CommandHandler $ \_ _ dt -> writeProp p dt v

hArg :: Int -> CommandHandler (Maybe String)
hArg n = CommandHandler $ \m _ _ -> pure $ let args = words (unpack (messageText m)) in if length args > n then Just (args !! n) else Nothing

hArgs :: CommandHandler [String]
hArgs = CommandHandler $ \m _ _ -> pure $ tail $ words (unpack (messageText m))

hData :: CommandHandler BotData
hData = CommandHandler $ \_ _ dt -> return dt

hDiscord :: DiscordHandler a -> CommandHandler a
hDiscord d = CommandHandler $ \_ _ _ -> d

hMDiscord :: ManagerT DiscordHandler a -> CommandHandler a
hMDiscord d = CommandHandler $ \_ man _ -> runManagerT d man

hMIO :: ManagerT IO a -> CommandHandler a
hMIO d = CommandHandler $ \_ man _ -> liftIO $ runManagerT d man

call :: (FromJSON a, R.Request (r a), NFData (r a)) => r a -> DiscordHandler (Either RestCallErrorCode a)
call r = liftIO (evaluate (force r)) >>= restCall

call_ :: (FromJSON a, R.Request (r a), NFData (r a)) => r a -> DiscordHandler ()
call_ r = void $ call r

respond :: Message -> String -> DiscordHandler ()
respond m s = call_ $ R.CreateMessage (messageChannel m) $ pack s

respondFile :: Message -> T.Text -> String -> DiscordHandler ()
respondFile m n s = call_ $ R.CreateMessageUploadFile (messageChannel m) n $ encodeUtf8 $ pack s