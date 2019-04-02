module Network.Telegram.API.Bot.Capacity.Editable (Editable (..), Substitution) where

import "aeson" Data.Aeson (FromJSON, Value, decode)
import "base" Control.Exception (try)
import "base" Control.Monad (join)
import "base" Data.Maybe (fromJust)
import "http-client" Network.HTTP.Client (Response (responseBody))
import "text" Data.Text (unpack)
import "transformers" Control.Monad.Trans.Class (lift)
import "transformers" Control.Monad.Trans.Except (ExceptT (ExceptT))
import "transformers" Control.Monad.Trans.Reader (ask)

import qualified "wreq" Network.Wreq.Session as Wreq (post)

import Network.Telegram.API.Bot.Core (Telegram, Token (Token), Ok, result)

type family Substitution a = r | r -> a

class FromJSON a => Editable a where
	{-# MINIMAL substitution_value, edit_endpoint #-}
	substitution_value :: Substitution a -> Value
	edit_endpoint :: Substitution a -> String

	edit :: Substitution a -> Telegram e ()
	edit x = snd <$> ask >>= \(session, Token token) -> lift . ExceptT . try
		. fmap (fromJust . join . fmap result . decode @(Ok ()) . responseBody)
			. flip (Wreq.post session) (substitution_value x) $
				"https://api.telegram.org/" <> unpack token <> "/" <> edit_endpoint x
