module Network.Telegram.API.Bot.Core (Telegram, ask', Token (..), telegram) where

import "base" Control.Exception (SomeException)
import "text" Data.Text (Text)
import "transformers" Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import "transformers" Control.Monad.Trans.Except (ExceptT, runExceptT)

newtype Token = Token Text deriving Eq

type Telegram e a = ReaderT (Token, e) (ExceptT SomeException IO) a

telegram :: Token -> e -> Telegram e a -> IO (Either SomeException a)
telegram token env = runExceptT . flip runReaderT (token, env)

ask' :: Telegram e e
ask' = snd <$> ask
