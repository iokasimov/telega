module Network.API.Telegram.Bot.Property.Persistable
	(Persistable (..), Capacity (..), Inform (..), Way (..)) where

import "aeson" Data.Aeson (FromJSON, Value (Object), Object, decode)
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
import "wreq" Network.Wreq.Session (post)

import Network.API.Telegram.Bot.Core (Telegram, Token (Token), Ok, result)

data Inform = Silently | Notify

data Way = Directly | Forwarding | Replying

data Capacity object = Send Inform Way object | Post object | Fetch object | Edit object | Purge object

class Persistable capacity object where
	{-# MINIMAL payload, endpoint #-}
	type Payload (capacity :: * -> Capacity *) object = payload | payload -> capacity object
	payload :: Payload capacity object -> Object
	endpoint :: Payload capacity object -> String
	persist :: FromJSON r => Payload capacity object -> Telegram e r
	persist x = request (endpoint x) (Object $ payload x) where

		request :: forall a e . FromJSON a => String -> Value -> Telegram e a
		request e p = snd <$> ask >>= \(session, Token token) -> lift . ExceptT . try
			. fmap (fromJust . join . fmap result . decode @(Ok a) . responseBody)
				. flip (post session) p $ "https://api.telegram.org/" <> unpack token <> "/" <> e
