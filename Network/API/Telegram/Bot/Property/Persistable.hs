module Network.API.Telegram.Bot.Property.Persistable
	(Persistable (..), Capacity (..), Inform (..), Way (..)) where

import "aeson" Data.Aeson (FromJSON, Value, decode, object, (.=))
import "base" Control.Exception (try)
import "base" Control.Monad (Monad ((>>=)), join)
import "base" Data.Bool (Bool (False, True))
import "base" Data.Function (flip, (.), ($))
import "base" Data.Functor (Functor (fmap), (<$>))
import "base" Data.Int (Int, Int64)
import "base" Data.Maybe (fromJust)
import "base" Data.Semigroup (Semigroup ((<>)))
import "base" Data.String (String)
import "base" Data.Tuple (snd)
import "http-client" Network.HTTP.Client (Response (responseBody))
import "tagged" Data.Tagged (Tagged, untag)
import "text" Data.Text (Text, unpack)
import "transformers" Control.Monad.Trans.Class (lift)
import "transformers" Control.Monad.Trans.Except (ExceptT (ExceptT))
import "transformers" Control.Monad.Trans.Reader (ask)
import "wreq" Network.Wreq.Session (post)

import Network.API.Telegram.Bot.Core (Telegram, Token (Token), Ok, result)
import Network.API.Telegram.Bot.Object (Object, Keyboard, Notification, Member, Sender)
import Network.API.Telegram.Bot.Object.Update.Message (Message)
import Network.API.Telegram.Bot.Object.Update.Message.Content.Location (Location (Location))

data Inform = Silently | Notify

data Way = Directly | Forwarding | Replying

data Capacity object = Send Inform Way object | Post object | Fetch object | Edit object | Purge object

class Object object => Persistable capacity object where
	{-# MINIMAL payload, endpoint #-}
	type Payload (capacity :: * -> Capacity *) object = payload | payload -> capacity object
	payload :: Payload capacity object -> Value
	endpoint :: Payload capacity object -> String
	request :: FromJSON r => Payload capacity object -> Telegram e r
	request x = request' (endpoint x) (payload x) where

		request' :: forall a e . FromJSON a => String -> Value -> Telegram e a
		request' e p = snd <$> ask >>= \(session, Token token) -> lift . ExceptT . try
			. fmap (fromJust . join . fmap result . decode @(Ok a) . responseBody)
				. flip (post session) p $ "https://api.telegram.org/" <> unpack token <> "/" <> e

instance Persistable 'Edit Keyboard where
	type instance Payload 'Edit Keyboard
		= Tagged ('Edit Keyboard) (Int64, Int, Keyboard)
	payload (untag -> (chat_id, message_id, reply_markup)) = object
		["chat_id" .= chat_id, "message_id" .= message_id, "reply_markup" .= reply_markup]
	endpoint _ = "editMessageReplyMarkup"

instance Persistable 'Post Keyboard where
	type instance Payload 'Post Keyboard
		= Tagged ('Post Keyboard) (Int64, Text, Keyboard)
	payload (untag -> (chat_id, text, kb)) = object
		["chat_id" .= chat_id, "text" .= text, "reply_markup" .= kb]
	endpoint _ = "sendMessage"

instance Persistable ('Send 'Notify 'Directly) Location where
	type instance Payload ('Send 'Notify 'Directly) Location
		= Tagged ('Send 'Notify 'Directly Location) (Int64, Location, Int)
	payload (untag -> (chat_id, Location latitude longitude, live_period)) =
		object ["chat_id" .= chat_id, "latitude" .= latitude, "longitude" .= longitude,
			"live_period" .= live_period, "disable_notification" .= False]
	endpoint _ = "sendLocation"

instance Persistable ('Send 'Silently 'Directly) Location where
	type instance Payload ('Send 'Silently 'Directly) Location
		= Tagged ('Send 'Silently 'Directly Location) (Int64, Location, Int)
	payload (untag -> (chat_id, Location latitude longitude, live_period)) =
		object ["chat_id" .= chat_id, "latitude" .= latitude, "longitude" .= longitude,
			"live_period" .= live_period, "disable_notification" .= True]
	endpoint _ = "sendLocation"

instance Persistable ('Send 'Notify 'Replying) Location where
	type instance Payload ('Send 'Notify 'Replying) Location
		= Tagged ('Send 'Notify 'Replying Location) (Int64, Location, Int, Int)
	payload (untag -> (chat_id, Location latitude longitude, live_period, reply_to_message_id)) = object
		["chat_id" .= chat_id, "latitude" .= latitude, "longitude" .= longitude, "live_period" .= live_period,
			"reply_to_message_id" .= reply_to_message_id, "disable_notification" .= False]
	endpoint _ = "sendLocation"

instance Persistable ('Send 'Silently 'Replying) Location where
	type instance Payload ('Send 'Silently 'Replying) Location
		= Tagged ('Send 'Silently 'Replying Location) (Int64, Location, Int, Int)
	payload (untag -> (chat_id, Location latitude longitude, live_period, reply_to_message_id)) = object
		["chat_id" .= chat_id, "latitude" .= latitude, "longitude" .= longitude, "live_period" .= live_period,
			"reply_to_message_id" .= reply_to_message_id, "disable_notification" .= True]
	endpoint _ = "sendLocation"

instance Persistable 'Fetch Member where
	type instance Payload 'Fetch Member = Tagged ('Fetch Member) (Int64, Int)
	payload (untag -> (chat_id, user_id)) = object ["chat_id" .= chat_id, "user_id" .= user_id]
	endpoint _ = "getChatMember"

instance Persistable ('Send 'Notify 'Directly) Message where
	type instance Payload ('Send 'Notify 'Directly) Message
		= Tagged ('Send 'Notify 'Directly Message) (Int64, Text)
	payload (untag -> (chat_id, text)) = object
		["chat_id" .= chat_id, "text" .= text, "disable_notification" .= False]
	endpoint _ = "sendMessage"

instance Persistable ('Send 'Silently 'Directly) Message where
	type instance Payload ('Send 'Silently 'Directly) Message
		= Tagged ('Send 'Silently 'Directly Message) (Int64, Text)
	payload (untag -> (chat_id, text)) = object
		["chat_id" .= chat_id, "text" .= text, "disable_notification" .= True]
	endpoint _ = "sendMessage"

instance Persistable ('Send 'Notify 'Forwarding) Message where
	type instance Payload ('Send 'Notify 'Forwarding) Message
		= Tagged ('Send 'Notify 'Forwarding Message) (Int64, Int64, Int)
	payload (untag -> (chat_id, from_chat_id, message_id)) = object
		["chat_id" .= chat_id, "from_chat_id" .= from_chat_id,
			"message_id" .= message_id, "disable_notification" .= False]
	endpoint _ = "forwardMessage"

instance Persistable ('Send 'Silently 'Forwarding) Message where
	type instance Payload ('Send 'Silently 'Forwarding) Message
		= Tagged ('Send 'Silently 'Forwarding Message) (Int64, Int64, Int)
	payload (untag -> (chat_id, from_chat_id, message_id)) = object
		["chat_id" .= chat_id, "from_chat_id" .= from_chat_id,
			"message_id" .= message_id, "disable_notification" .= True]
	endpoint _ = "forwardMessage"

instance Persistable ('Send 'Notify 'Replying) Message where
	type instance Payload ('Send 'Notify 'Replying) Message
		= Tagged ('Send 'Notify 'Replying Message) (Int64, Int, Text)
	payload (untag -> (chat_id, reply_to_message_id, text)) = object
		["chat_id" .= chat_id, "reply_to_message_id" .= reply_to_message_id,
			"text" .= text, "disable_notification" .= False]
	endpoint _ = "sendMessage"

instance Persistable ('Send 'Silently 'Replying) Message where
	type instance Payload ('Send 'Silently 'Replying) Message
		= Tagged ('Send 'Silently 'Replying Message) (Int64, Int, Text)
	payload (untag -> (chat_id, reply_to_message_id, text)) = object
		["chat_id" .= chat_id, "reply_to_message_id" .= reply_to_message_id,
			"text" .= text, "disable_notification" .= True]
	endpoint _ = "sendMessage"

instance Persistable 'Edit Message where
	type instance Payload 'Edit Message = Tagged ('Edit Message) (Int64, Int, Text)
	payload (untag -> (chat_id, message_id, text)) = object
		["chat_id" .= chat_id, "message_id" .= message_id, "text" .= text]
	endpoint _ = "editMessageText"

instance Persistable 'Purge Message where
	type instance Payload 'Purge Message = Tagged ('Purge Message) (Int64, Int)
	payload (untag -> (chat_id, message_id)) = object ["chat_id" .= chat_id, "message_id" .= message_id]
	endpoint _ = "deleteMessage"
--
instance Persistable 'Post Notification where
	type instance Payload 'Post Notification = Tagged ('Post Notification) (Text, Text)
	payload (untag -> (cbq_id, text)) = object ["callback_query_id" .= cbq_id, "text" .= text]
	endpoint _ = "answerCallbackQuery"
--
instance Persistable 'Fetch Sender where
	type instance Payload 'Fetch Sender = Tagged ('Fetch Sender) ()
	payload _ = object []
	endpoint _ = "getMe"
