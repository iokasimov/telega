module Network.Telegram.API.Bot.Property.Persistable
	(Persistable (..), Payload, PL (..), Capacity (..), Message' (..)) where

import "aeson" Data.Aeson (FromJSON, Value, decode, object, (.=))
import "base" Control.Exception (try)
import "base" Control.Monad (Monad ((>>=)), join)
import "base" Data.Function (flip, (.), ($))
import "base" Data.Functor (Functor (fmap), (<$>))
import "base" Data.Int (Int, Int64)
import "base" Data.Maybe (Maybe, fromJust)
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
import Network.Telegram.API.Bot.Object (Object, Keyboard, Notification, Member, Sender)
import Network.Telegram.API.Bot.Object.Update.Message (Message)
import Network.Telegram.API.Bot.Object.Update.Message.Content.Info (Info)
import Network.Telegram.API.Bot.Object.Update.Message.Content.Location (Location)

data Capacity = Post | Fetch | Edit | Purge

newtype PL c o a = PL a

type family Payload (c :: k) o = r | r -> o c

type instance Payload 'Post Keyboard = PL 'Post Keyboard (Int64, Text, Keyboard)
type instance Payload 'Edit Keyboard = PL 'Edit Keyboard (Int64, Int, Keyboard)
type instance Payload 'Fetch Member = PL 'Fetch Member (Int64, Int)
type instance Payload 'Edit Message = PL 'Edit Message (Int64, Int, Text)
type instance Payload 'Purge Message = PL 'Purge Message (Int64, Int)
type instance Payload 'Post Notification = PL 'Post Notification (Text, Text)
type instance Payload 'Fetch Sender = PL 'Fetch Sender ()

data Message' = Direct' Capacity | Forward' Capacity | Reply' Capacity

type instance Payload ('Direct' 'Post) Message = PL ('Direct' 'Post) Message (Int64, Text)
type instance Payload ('Forward' 'Post) Message = PL ('Forward' 'Post) Message (Int64, Int64, Int)
type instance Payload ('Reply' 'Post) Message = PL ('Reply' 'Post) Message (Int64, Int, Text)

data Info' = Point' Message' | Contact' Message' | Venue' Message'

type instance Payload ('Point' ('Direct' 'Post)) Info = PL ('Point' ('Direct' 'Post)) Info (Int64, Location, Int)
type instance Payload ('Contact' ('Direct' 'Post)) Info = PL ('Contact' ('Direct' 'Post)) Info (Int64, Text, Text, Maybe Text, Maybe Text)
type instance Payload ('Venue' ('Direct' 'Post)) Info = PL ('Venue' ('Direct' 'Post)) Info (Int64, Location, Text, Text, Maybe Text, Maybe Text)

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

instance Persistable 'Edit Keyboard where
	payload (PL (chat_id, message_id, reply_markup)) = object
		["chat_id" .= chat_id, "message_id" .= message_id, "reply_markup" .= reply_markup]
	endpoint _ = "editMessageReplyMarkup"

instance Persistable 'Post Keyboard where
	payload (PL (chat_id, text, kb)) = object
		["chat_id" .= chat_id, "text" .= text, "reply_markup" .= kb]
	endpoint _ = "sendMessage"

instance Persistable 'Fetch Member where
	payload (PL (chat_id, user_id)) = object ["chat_id" .= chat_id, "user_id" .= user_id]
	endpoint _ = "getChatMember"

instance Persistable ('Direct' 'Post) Message where
	payload (PL (chat_id, text)) = object ["chat_id" .= chat_id, "text" .= text]
	endpoint _ = "sendMessage"

instance Persistable ('Forward' 'Post) Message where
	payload (PL (chat_id, from_chat_id, message_id)) = object
		["chat_id" .= chat_id, "from_chat_id" .= from_chat_id, "message_id" .= message_id]
	endpoint _ = "forwardMessage"

instance Persistable ('Reply' 'Post) Message where
	payload (PL (chat_id, reply_to_message_id, text)) = object
		["chat_id" .= chat_id, "reply_to_message_id" .= reply_to_message_id, "text" .= text]
	endpoint _ = "sendMessage"

instance Persistable 'Purge Message where
	payload (PL (chat_id, message_id)) = object ["chat_id" .= chat_id, "message_id" .= message_id]
	endpoint _ = "deleteMessage"

instance Persistable 'Post Notification where
	payload (PL (cbq_id, text)) = object ["callback_query_id" .= cbq_id, "text" .= text]
	endpoint _ = "answerCallbackQuery"

instance Persistable 'Fetch Sender where
	payload (PL ()) = object []
	endpoint _ = "getMe"

instance Persistable 'Edit Message where
	payload (PL (chat_id, message_id, text)) = object
		["chat_id" .= chat_id, "message_id" .= message_id, "text" .= text]
	endpoint _ = "editMessageText"

instance Persistable ('Point' ('Direct' 'Post)) Info where
	payload (PL (chat_id, location, live_period)) = object
		["chat_id" .= chat_id, "location" .= location, "live_period" .= live_period]
	endpoint _ = "sendLocation"

instance Persistable ('Contact' ('Direct' 'Post)) Info where
	payload (PL (chat_id, phone_number, first_name, last_name, vcard)) =
		object ["chat_id" .= chat_id, "phone_number" .= phone_number,
			"first_name" .= first_name, "last_name" .= last_name, "vcard" .= vcard]
	endpoint _ = "sendContact"

instance Persistable ('Venue' ('Direct' 'Post)) Info where
	payload (PL (chat_id, location, title, address, foursquare_id, foursquare_type)) = object
		["chat_id" .= chat_id, "location" .= location, "title" .= title, "address" .= address,
			"foursquare_id" .= foursquare_id, "foursquare_type" .= foursquare_type]
	endpoint _ = "sendVenue"
