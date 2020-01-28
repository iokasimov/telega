module Network.API.Telegram.Bot.Property.Persistable (Persistable (..)) where

import "aeson" Data.Aeson (FromJSON, Object, Value (Object))
import "base" Control.Exception (SomeException, try)
import "base" Control.Monad ((>>=))
import "base" Data.Function ((.), ($))
import "base" Data.Functor ((<$>))
import "base" Data.Maybe (fromJust)
import "base" Data.Monoid (mempty)
import "base" Data.Tuple (snd)
import "base" System.IO (IO)
import "data-default" Data.Default (def)
import "joint" Control.Joint (Reader, get, lift)
import "text" Data.Text (Text)
import "req" Network.HTTP.Req (POST (POST), ReqBodyJson (ReqBodyJson)
	, https, jsonResponse, req, responseBody, runReq, (/:))

import Network.API.Telegram.Bot.Core (Telegram, Token (Token), Ok, result)

class Persistable action where
	{-# MINIMAL payload, endpoint #-}
	type Payload action = payload | payload -> action
	type Returning action :: *
	payload :: Payload action -> Object
	endpoint :: Payload action -> Text

	persist :: FromJSON (Returning action) => Payload action -> Telegram e (Returning action)
	persist x = request (endpoint x) (Object $ payload x)

	persist_ :: Payload action -> Telegram e ()
	persist_ x = request @() @_ (endpoint x) (Object $ payload x)

request :: forall a e . FromJSON a => Text -> Value -> Telegram e a
request e p = snd <$> lift @(Reader (e, Token)) get >>= lift . try @SomeException . send >>= lift where

	send :: FromJSON a => Token -> IO a
	send (Token token) = (<$>) (fromJust . result . responseBody) . runReq def
		$ req POST (https "api.telegram.org" /: token /: e)
			(ReqBodyJson p) (jsonResponse @(Ok a)) mempty
