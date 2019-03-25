module Network.Telegram.API.Bot.Core (Telegram, ask', Token (..), telegram) where

import "base" Control.Exception (SomeException)
import "text" Data.Text (Text)
import "transformers" Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import "transformers" Control.Monad.Trans.Except (ExceptT, runExceptT)
import "wreq" Network.Wreq.Session (Session)

newtype Token = Token Text deriving Eq

type Telegram e a = ReaderT (e, (Session, Token)) (ExceptT SomeException IO) a

telegram :: Session -> Token -> e -> Telegram e a -> IO (Either SomeException a)
telegram session token env = runExceptT . flip runReaderT (env, (session, token))

ask' :: Telegram e e
ask' = fst <$> ask
