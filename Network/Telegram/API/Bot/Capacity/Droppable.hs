module Network.Telegram.API.Bot.Capacity.Droppable (Droppable (..), Drop) where

import "aeson" Data.Aeson (Value, decode)
import "base" Control.Exception (try)
import "base" Control.Monad (Monad ((>>=)), join, void)
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

type family Drop a = r | r -> a

class Droppable a where
	{-# MINIMAL drop_value, drop_endpoint #-}
	drop_value :: Drop a -> Value
	drop_endpoint :: Drop a -> String

	drop :: Drop a -> Telegram e ()
	drop x = snd <$> ask >>= \(session, Token token) -> void . lift . ExceptT . try
		. fmap (fromJust . join . fmap result . decode @(Ok ()) . responseBody)
			. flip (Wreq.post session) (drop_value x) $
				"https://api.telegram.org/" <> unpack token <> "/" <> drop_endpoint x
