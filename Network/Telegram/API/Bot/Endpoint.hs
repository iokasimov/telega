module Network.Telegram.API.Bot.Endpoint
	(Endpoint (..), Payload, Edit, Post, Purge) where

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
import "wreq" Network.Wreq.Session (post)

import Network.Telegram.API.Bot.Core (Telegram, Token (Token), Ok, result)

type family Payload a = r | r -> a

data Edit a
data Post a
data Purge a

class Endpoint a where
	{-# MINIMAL payload, endpoint #-}
	payload :: Payload a -> Value
	endpoint :: Payload a -> String
	request :: FromJSON r => Payload a -> Telegram e r
	request x = request' (endpoint x) (payload x)

request' :: forall a env . FromJSON a => String -> Value -> Telegram env a
request' e p = snd <$> ask >>= \(session, Token token) -> lift . ExceptT . try
	. fmap (fromJust . join . fmap result . decode @(Ok a) . responseBody)
		. flip (post session) p $ "https://api.telegram.org/" <> unpack token <> "/" <> e
