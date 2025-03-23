module Network.API.Telegram.Bot.Property.Persistable (Persistable (..)) where

import "aeson" Data.Aeson (FromJSON, Object, Value (Object))
import "base" Control.Exception (SomeException, try)
import "base" Control.Monad ((>>=))
import "base" Data.Function ((.), ($))
import "base" Data.Functor ((<$>))
import "base" Data.Maybe (fromJust)
import "base" Data.Monoid (mempty, (<>))
import "base" Data.Tuple (snd)
import "base" System.IO (IO)
import "text" Data.Text (Text)
import "transformers" Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT, except)
import "transformers" Control.Monad.Trans.Reader (ask, runReaderT)
import "transformers" Control.Monad.Trans.Class (lift)

import "req" Network.HTTP.Req (POST (POST), ReqBodyJson (ReqBodyJson)
	, https, jsonResponse, req, responseBody, runReq, (/:), defaultHttpConfig)

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
request e p = snd <$> ask >>= lift . ExceptT . try @SomeException . send where

	send :: FromJSON a => Token -> IO a
	send (Token token) = (<$>) (fromJust . result . responseBody) . runReq defaultHttpConfig
		$ req POST (https "api.telegram.org" /: ("bot" <> token) /: e)
			(ReqBodyJson p) (jsonResponse @(Ok a)) mempty
