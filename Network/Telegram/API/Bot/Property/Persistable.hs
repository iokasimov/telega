module Network.Telegram.API.Bot.Property.Persistable (Persistable (..), Payload, Capacity (..)) where

import "aeson" Data.Aeson (FromJSON, Value, decode, object, (.=))
import "base" Control.Exception (try)
import "base" Control.Monad (Monad ((>>=)), join)
import "base" Data.Function (flip, (.), ($))
import "base" Data.Functor (Functor (fmap), (<$>))
import "base" Data.Int (Int, Int64)
import "base" Data.Maybe (Maybe (Just, Nothing), fromJust)
import "base" Data.Semigroup (Semigroup ((<>)))
import "base" Data.String (String)
import "base" Data.Tuple (snd)
import "http-client" Network.HTTP.Client (Response (responseBody))
import "text" Data.Text (Text, unpack)
import "transformers" Control.Monad.Trans.Class (lift)
import "transformers" Control.Monad.Trans.Except (ExceptT (ExceptT))
import "transformers" Control.Monad.Trans.Reader (ask)
import "wreq" Network.Wreq.Session (post)

import Network.Telegram.API.Bot.Core (Telegram, Token (Token), Ok, result)
import Network.Telegram.API.Bot.Object (Object)
import Network.Telegram.API.Bot.Object.Callback (Notification)
import Network.Telegram.API.Bot.Object.Message (Keyboard, Message)

data Capacity = Post | Edit | Purge

type family Payload (c :: Capacity) o = r | r -> c o

class Object o => Persistable c o where
	{-# MINIMAL payload, endpoint #-}
	payload :: Payload c o -> Value
	endpoint :: Payload c o -> String
	request :: FromJSON r => Payload c o -> Telegram e r
	request x = request' (endpoint x) (payload x) where

		request' :: forall a e . FromJSON a => String -> Value -> Telegram e a
		request' e p = snd <$> ask >>= \(session, Token token) -> lift . ExceptT . try
			. fmap (fromJust . join . fmap result . decode @(Ok a) . responseBody)
				. flip (post session) p $ "https://api.telegram.org/" <> unpack token <> "/" <> e

type instance Payload 'Edit Keyboard = (Int64, Int, Keyboard)
type instance Payload 'Post Message = (Int64, Text, Maybe Keyboard)
type instance Payload 'Purge Message = (Int64, Int)
type instance Payload 'Post Notification = (Text, Text)

instance Persistable 'Edit Keyboard where
	payload (chat_id, message_id, reply_markup) = object
		["chat_id" .= chat_id, "message_id" .= message_id, "reply_markup" .= reply_markup]
	endpoint _ = "editMessageReplyMarkup"

instance Persistable 'Post Message where
	payload (chat_id, text, Nothing) = object ["chat_id" .= chat_id, "text" .= text]
	payload (chat_id, text, Just kb) = object ["chat_id" .= chat_id, "text" .= text, "reply_markup" .= kb]
	endpoint _ = "sendMessage"

instance Persistable 'Purge Message where
	payload (chat_id, message_id) = object ["chat_id" .= chat_id, "message_id" .= message_id]
	endpoint _ = "deleteMessage"

instance Persistable 'Post Notification where
	payload (cbq_id, text) = object ["callback_query_id" .= cbq_id, "text" .= text]
	endpoint _ = "answerCallbackQuery"
