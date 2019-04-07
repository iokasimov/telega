module Network.Telegram.API.Bot.Internal (telegram_request) where

import "aeson" Data.Aeson (FromJSON, Value, decode)
import "base" Control.Exception (try)
import "base" Control.Monad (Monad ((>>=)), join)
import "base" Data.Function (flip, (.), ($))
import "base" Data.Functor (Functor (fmap), (<$>))
import "base" Data.Maybe (fromJust)
import "base" Data.Semigroup (Semigroup ((<>)))
import "base" Data.String (String)
import "base" Data.Tuple (snd)
import "http-client" Network.HTTP.Client (Response (responseBody))
import "text" Data.Text (unpack)
import "transformers" Control.Monad.Trans.Class (lift)
import "transformers" Control.Monad.Trans.Except (ExceptT (ExceptT))
import "transformers" Control.Monad.Trans.Reader (ask)

import qualified "wreq" Network.Wreq.Session as Wreq (post)

import Network.Telegram.API.Bot.Core (Telegram, Token (Token), Ok, result)

telegram_request :: forall a e . FromJSON a => String -> Value -> Telegram e a
telegram_request endpoint payload = snd <$> ask >>= \(session, Token token) -> lift . ExceptT . try
	. fmap (fromJust . join . fmap result . decode @(Ok a) . responseBody)
		. flip (Wreq.post session) payload $ "https://api.telegram.org/" <> unpack token <> "/" <> endpoint
