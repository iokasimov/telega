module Network.Telegram.API.Bot (module Exports, Telegram, ask', Token (Token)) where

import "http-client" Network.HTTP.Client (Manager)
import "servant" Servant.API (Capture, FromHttpApiData, ToHttpApiData (toUrlPiece))
import "servant-client" Servant.Client (BaseUrl (BaseUrl), ClientEnv, ClientM
	, ServantError, Scheme (Https), mkClientEnv, runClientM)
import "text" Data.Text (Text, unpack)
import "transformers" Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)

import Network.Telegram.API.Bot.Chat as Exports
import Network.Telegram.API.Bot.From as Exports
import Network.Telegram.API.Bot.Message as Exports
import Network.Telegram.API.Bot.Update as Exports

newtype Token = Token Text deriving (Eq, FromHttpApiData, ToHttpApiData)

type Telegram e a = ReaderT (Token, e) ClientM a

telegram :: Manager -> Token -> e -> Telegram e a -> IO (Either ServantError a)
telegram manager token env = flip runClientM (mkClientEnv manager $ base_url token) . flip runReaderT (token, env) where

	base_url :: Token -> BaseUrl
	base_url = BaseUrl Https "api.telegram.org" 443 . unpack . toUrlPiece

ask' :: Telegram e e
ask' = snd <$> ask
