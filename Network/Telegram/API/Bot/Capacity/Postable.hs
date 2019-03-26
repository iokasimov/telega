module Network.Telegram.API.Bot.Capacity.Postable (Ok (..), Postable (..), Initial) where

import "aeson" Data.Aeson (FromJSON (parseJSON), Value, decode, withObject, (.:))
import "base" Control.Exception (try)
import "base" Control.Monad (join)
import "base" Data.Maybe (fromJust)
import "http-client" Network.HTTP.Client (Response (responseBody))
import "text" Data.Text (unpack)
import "transformers" Control.Monad.Trans.Class (lift)
import "transformers" Control.Monad.Trans.Except (ExceptT (ExceptT))
import "transformers" Control.Monad.Trans.Reader (ask)

import qualified "wreq" Network.Wreq.Session as Wreq (post)

import Network.Telegram.API.Bot.Core (Telegram, Token (Token))

type family Initial a = r | r -> a

class FromJSON a => Postable a where
	{-# MINIMAL payload, endpoint #-}
	payload :: Initial a -> Value
	endpoint :: Initial a -> String

	post :: Initial a -> Telegram e a
	post x = snd <$> ask >>= \(session, Token token) -> lift . ExceptT . try
		. fmap (fromJust . join . fmap result . decode @(Ok a) . responseBody)
			. flip (Wreq.post session) (payload x) $
				"https://api.telegram.org/" <> unpack token <> "/" <> endpoint x

data Ok a = Ok Bool a
	deriving Show

result :: Ok a -> Maybe a
result (Ok True x) = Just x
result (Ok False _ ) = Nothing

instance FromJSON a => FromJSON (Ok a) where
	parseJSON = withObject "Ok" $ \v ->
		Ok <$> v .: "ok" <*> v .: "result"
