module Network.API.Telegram.Bot.Property.Persistable
	(Persistable (..), Capacity (..), Silenlty (..)) where
	-- (Persistable (..), Capacity (..), Inform (..), Way (..)) where

import "aeson" Data.Aeson (FromJSON, ToJSON (toJSON), Value (Object), Object, decode)
import "base" Control.Exception (try)
import "base" Control.Monad (Monad ((>>=)), join)
import "base" Data.Bool (Bool (True))
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
import "unordered-containers" Data.HashMap.Strict (singleton)
import "with" Data.With (type (:&:)((:&:)))
import "wreq" Network.Wreq.Session (post)

import Network.API.Telegram.Bot.Core (Telegram, Token (Token), Ok, result)

data Capacity object = Send object

data Silenlty (capacity :: * -> Capacity *) object = Silenlty

class Persistable action where
	{-# MINIMAL payload, endpoint #-}
	type Payload action = payload | payload -> action
	payload :: Payload action -> Object
	endpoint :: Payload action -> String
	persist :: FromJSON r => Payload action -> Telegram e r
	persist x = request (endpoint x) (Object $ payload x) where

		request :: forall a e . FromJSON a => String -> Value -> Telegram e a
		request e p = snd <$> ask >>= \(session, Token token) -> lift . ExceptT . try
			. fmap (fromJust . join . fmap result . decode @(Ok a) . responseBody)
				. flip (post session) p $ "https://api.telegram.org/" <> unpack token <> "/" <> e

instance Persistable ('Send obj) => Persistable (Silenlty 'Send obj) where
	type Payload (Silenlty 'Send obj) = (Silenlty 'Send obj :&: Payload ('Send obj))
	payload (_ :&: x) = payload x <> singleton "disable_notification" (toJSON True)
	endpoint (_ :&: x) = endpoint x
