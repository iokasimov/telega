module Network.Telegram.API.Bot.Capacity.Postable (Postable (..), Payload (..)) where

import "aeson" Data.Aeson (FromJSON, Value, decode)
import "base" Control.Exception (try)
import "base" Data.Maybe (fromJust)
import "http-client" Network.HTTP.Client (Response (responseBody))
import "text" Data.Text (unpack)
import "transformers" Control.Monad.Trans.Class (lift)
import "transformers" Control.Monad.Trans.Except (ExceptT (ExceptT))
import "transformers" Control.Monad.Trans.Reader (ask)
import qualified "wreq" Network.Wreq as Wreq (post)

import Network.Telegram.API.Bot.Core (Telegram, Token (Token))

class FromJSON a => Postable a where
	{-# MINIMAL payload, endpoint #-}
	data Payload a :: *
	payload :: Payload a -> Value
	endpoint :: Payload a -> String

	post :: Payload a -> Telegram e a
	post x = ask >>= \(Token token, _) -> lift . ExceptT . try
		. fmap (fromJust . decode . responseBody) . flip Wreq.post (payload x) $
			"https://api.telegram.org/" <> unpack token <> "/" <> endpoint x
