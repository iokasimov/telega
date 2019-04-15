module Network.API.Telegram.Bot.Property.Persistable
	(Persistable (..), Payload, Capacity (..), Way (..)) where

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
import "tagged" Data.Tagged (Tagged (Tagged), untag)
import "text" Data.Text (Text, unpack)
import "transformers" Control.Monad.Trans.Class (lift)
import "transformers" Control.Monad.Trans.Except (ExceptT (ExceptT))
import "transformers" Control.Monad.Trans.Reader (ask)
import "wreq" Network.Wreq.Session (post)

import Network.API.Telegram.Bot.Core (Telegram, Token (Token), Ok, result)
import Network.API.Telegram.Bot.Object (Object, Keyboard, Notification, Member, Sender)
import Network.API.Telegram.Bot.Object.Update.Message (Message)
import Network.API.Telegram.Bot.Object.Update.Message.Content.Info (Info)
import Network.API.Telegram.Bot.Object.Update.Message.Content.Location (Location)

data Way = Directly | Forwarding | Replying

data Capacity object = Send Way object | Post object | Fetch object | Edit object | Purge object

type family Payload (capacity :: * -> Capacity *) object = payload | payload -> capacity object

type instance Payload 'Post Keyboard = Tagged ('Post Keyboard) (Int64, Text, Keyboard)
type instance Payload 'Edit Keyboard = Tagged ('Edit Keyboard) (Int64, Int, Keyboard)
type instance Payload 'Fetch Member = Tagged ('Fetch Member) (Int64, Int)
type instance Payload ('Send 'Directly) Message = Tagged ('Send 'Directly Message) (Int64, Text)
type instance Payload ('Send 'Forwarding) Message = Tagged ('Send 'Forwarding Message) (Int64, Int64, Int)
type instance Payload ('Send 'Replying) Message = Tagged ('Send 'Replying Message) (Int64, Int, Text)
type instance Payload 'Edit Message = Tagged ('Edit Message) (Int64, Int, Text)
type instance Payload 'Purge Message = Tagged ('Purge Message) (Int64, Int)
type instance Payload 'Post Notification = Tagged ('Post Notification) (Text, Text)
type instance Payload 'Fetch Sender = Tagged ('Fetch Sender) ()

-- data Info' = Point' Way | Contact' Way | Venue' Way

-- type instance Payload ('Point' ('Send Directly)) Info= PL ('Point' ('Send Directly)) Info (Int64, Location, Int)
-- type instance Payload ('Contact' ('Send Directly)) Info = PL ('Contact' ('Send Directly)) Info (Int64, Text, Text, Maybe Text, Maybe Text)
-- type instance Payload ('Venue' ('Send Directly)) Info = PL ('Venue' ('Send Directly)) Info (Int64, Location, Text, Text, Maybe Text, Maybe Text)
-- type instance Payload ('Point' ('Send Replying)) Info = PL ('Point' ('Send Replying)) Info (Int64, Int, Location, Int)
-- type instance Payload ('Contact' ('Send Replying)) Info = PL ('Contact' ('Send Replying)) Info (Int64, Int, Text, Text, Maybe Text, Maybe Text)
-- type instance Payload ('Venue' ('Send Replying)) Info = PL ('Venue' ('Send Replying)) Info (Int64, Int, Location, Text, Text, Maybe Text, Maybe Text)

-- data Member' = Kick' | Unban'

-- type instance Payload 'Kick' Member = PL 'Kick' Member (Int64, Int, Int)
-- type instance Payload 'Unban' Member = PL 'Unban' Member (Int64, Int)

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
	payload (untag -> (chat_id, message_id, reply_markup)) = object
		["chat_id" .= chat_id, "message_id" .= message_id, "reply_markup" .= reply_markup]
	endpoint _ = "editMessageReplyMarkup"

instance Persistable 'Post Keyboard where
	payload (untag -> (chat_id, text, kb)) = object
		["chat_id" .= chat_id, "text" .= text, "reply_markup" .= kb]
	endpoint _ = "sendMessage"

instance Persistable 'Fetch Member where
	payload (untag -> (chat_id, user_id)) = object ["chat_id" .= chat_id, "user_id" .= user_id]
	endpoint _ = "getChatMember"

instance Persistable ('Send 'Directly) Message where
	payload (untag -> (chat_id, text)) = object ["chat_id" .= chat_id, "text" .= text]
	endpoint _ = "sendMessage"

instance Persistable ('Send 'Forwarding) Message where
	payload (untag -> (chat_id, from_chat_id, message_id)) = object
		["chat_id" .= chat_id, "from_chat_id" .= from_chat_id, "message_id" .= message_id]
	endpoint _ = "forwardMessage"

instance Persistable ('Send 'Replying) Message where
	payload (untag -> (chat_id, reply_to_message_id, text)) = object
		["chat_id" .= chat_id, "reply_to_message_id" .= reply_to_message_id, "text" .= text]
	endpoint _ = "sendMessage"

instance Persistable 'Purge Message where
	payload (untag -> (chat_id, message_id)) = object ["chat_id" .= chat_id, "message_id" .= message_id]
	endpoint _ = "deleteMessage"

instance Persistable 'Post Notification where
	payload (untag -> (cbq_id, text)) = object ["callback_query_id" .= cbq_id, "text" .= text]
	endpoint _ = "answerCallbackQuery"

instance Persistable 'Fetch Sender where
	payload _ = object []
	endpoint _ = "getMe"

instance Persistable 'Edit Message where
	payload (untag -> (chat_id, message_id, text)) = object
		["chat_id" .= chat_id, "message_id" .= message_id, "text" .= text]
	endpoint _ = "editMessageText"

-- instance Persistable ('Point' (Send 'Directly)) Info where
-- 	payload (PL (chat_id, location, live_period)) = object
-- 		["chat_id" .= chat_id, "location" .= location, "live_period" .= live_period]
-- 	endpoint _ = "sendLocation"
--
-- instance Persistable ('Contact' (Send 'Directly)) Info where
-- 	payload (PL (chat_id, phone_number, first_name, last_name, vcard)) =
-- 		object ["chat_id" .= chat_id, "phone_number" .= phone_number,
-- 			"first_name" .= first_name, "last_name" .= last_name, "vcard" .= vcard]
-- 	endpoint _ = "sendContact"
--
-- instance Persistable ('Venue' (Send 'Directly)) Info where
-- 	payload (PL (chat_id, location, title, address, foursquare_id, foursquare_type)) = object
-- 		["chat_id" .= chat_id, "location" .= location, "title" .= title, "address" .= address,
-- 			"foursquare_id" .= foursquare_id, "foursquare_type" .= foursquare_type]
-- 	endpoint _ = "sendVenue"
--
-- instance Persistable ('Point' (Send 'Replying)) Info where
-- 	payload (PL (chat_id, reply_to_message_id, location, live_period)) = object
-- 		["chat_id" .= chat_id, "reply_to_message_id" .= reply_to_message_id,
-- 			"location" .= location, "live_period" .= live_period]
-- 	endpoint _ = "sendLocation"
--
-- instance Persistable ('Contact' (Send 'Replying)) Info where
-- 	payload (PL (chat_id, reply_to_message_id, phone_number, first_name, last_name, vcard)) = object
-- 		["chat_id" .= chat_id, "reply_to_message_id" .= reply_to_message_id, "phone_number" .= phone_number,
-- 			"first_name" .= first_name, "last_name" .= last_name, "vcard" .= vcard]
-- 	endpoint _ = "sendContact"
--
-- instance Persistable ('Venue' (Send 'Replying)) Info where
-- 	payload (PL (chat_id, reply_to_message_id, location, title, address, foursquare_id, foursquare_type)) = object
-- 		["chat_id" .= chat_id, "reply_to_message_id" .= reply_to_message_id, "location" .= location, "title" .= title,
-- 			"address" .= address, "foursquare_id" .= foursquare_id, "foursquare_type" .= foursquare_type]
-- 	endpoint _ = "sendVenue"
--
-- instance Persistable 'Kick' Member where
-- 	payload (PL (chat_id, user_id, until_date)) = object
-- 		["chat_id" .= chat_id, "user_id" .= user_id, "until_date" .= until_date]
-- 	endpoint _ = "kickChatMember"
--
-- instance Persistable 'Unban' Member where
-- 	payload (PL (chat_id, user_id)) = object ["chat_id" .= chat_id, "user_id" .= user_id]
-- 	endpoint _ = "unbanChatMember"
