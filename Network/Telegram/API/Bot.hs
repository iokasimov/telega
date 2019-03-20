module Network.Telegram.API.Bot (module Exports, Telegram, Token) where

import "servant" Servant.API (FromHttpApiData, ToHttpApiData)
import "servant-client" Servant.Client (ClientM)
import "text" Data.Text (Text)
import "transformers" Control.Monad.Trans.Reader (ReaderT)

import Network.Telegram.API.Bot.Chat as Exports
import Network.Telegram.API.Bot.From as Exports
import Network.Telegram.API.Bot.Message as Exports

newtype Token = Token Text deriving (Eq, FromHttpApiData, ToHttpApiData)

type Telegram e a = ReaderT (Token, e) ClientM a
