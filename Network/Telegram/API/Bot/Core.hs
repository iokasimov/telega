module Network.Telegram.API.Bot.Core (Telegram, ask', Token (Token)) where

import "base" Control.Exception (SomeException)
import "http-client" Network.HTTP.Client (Manager)
import "servant" Servant.API (Capture, FromHttpApiData, ToHttpApiData (toUrlPiece))
import "servant-client" Servant.Client (BaseUrl (BaseUrl), ClientEnv, ClientM
	, ServantError, Scheme (Https), mkClientEnv, runClientM)
import "text" Data.Text (Text, unpack)
import "transformers" Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import "transformers" Control.Monad.Trans.Except (ExceptT)

newtype Token = Token Text deriving Eq

type Telegram e a = ReaderT (Token, e) (ExceptT SomeException IO) a

ask' :: Telegram e e
ask' = snd <$> ask
