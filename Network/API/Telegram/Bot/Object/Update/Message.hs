module Network.API.Telegram.Bot.Object.Update.Message (module Exports, Message (..)
	, Send (..), Reply (..), Forward (..), Edit (..), Delete (..), Silently (..)) where

import Network.API.Telegram.Bot.Object.Update.Message.Content as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Keyboard as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Origin as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object), withObject, (.:))
import "aeson" Data.Aeson.Types (Object, Parser)
import "base" Control.Applicative (Applicative ((<*>)), Alternative ((<|>)))
import "base" Control.Monad (Monad ((>>=)), fail)
import "base" Data.Bool (Bool (True))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int, Int64)
import "base" Data.Semigroup ((<>))
import "base" Text.Show (Show)
import "text" Data.Text (Text)
import "with" Data.With (type (:&:)((:&:)))

import Network.API.Telegram.Bot.Object.Update.Message.Content (Content)
import Network.API.Telegram.Bot.Object.Update.Message.Origin (Origin (Private, Group, Supergroup, Channel))
import Network.API.Telegram.Bot.Property.Accessible (Accessible (access))
import Network.API.Telegram.Bot.Property.Identifiable (Identifiable (Identificator, ident))
import Network.API.Telegram.Bot.Property.Persistable (Persistable (Payload, payload, endpoint))
import Network.API.Telegram.Bot.Utils (field)

data Message
	= Direct Int Origin Content
	| Forwarded Int Origin Content
	| Replied Int Origin Content Message
	deriving Show

instance Accessible Content Message where
	access f (Direct msg_id origin content) = (\content' -> Direct msg_id origin content') <$> f content
	access f (Forwarded msg_id origin content) = (\content' -> Forwarded msg_id origin content') <$> f content
	access f (Replied msg_id origin content msg) = (\content' -> Replied msg_id origin content' msg) <$> f content

instance Accessible Origin Message where
	access f (Direct msg_id origin content) = (\origin' -> Direct msg_id origin' content) <$> f origin
	access f (Forwarded msg_id origin content) = (\origin' -> Forwarded msg_id origin' content) <$> f origin
	access f (Replied msg_id origin content msg) = (\origin' -> Replied msg_id origin' content msg) <$> f origin

instance FromJSON Message where
	parseJSON = withObject "Message" $ \v ->
		forward_channel v <|> forward_chat v <|> reply v <|> direct v where

		forward_channel :: Object -> Parser Message
		forward_channel v = Forwarded <$> v .: "forward_from_message_id"
			<*> (v .: "forward_from_chat" >>= channel) <*> parseJSON (Object v) where

			channel :: Value -> Parser Origin
			channel = withObject "Channel" $ \c -> Channel <$> c .: "id" <*> c .: "title"

		forward_chat :: Object -> Parser Message
		forward_chat v = Forwarded <$> v .: "message_id"
			<*> (v .: "chat" >>= chat) <*> parseJSON (Object v) where

			chat :: Value -> Parser Origin
			chat = withObject "Origin" $ \c -> c .: "type" >>= \case
				("private" :: Text) -> Private <$> c .: "id" <*> v .: "forward_from"
				("group" :: Text) -> Group <$> c .: "id" <*> c .: "title" <*> v .: "forward_from"
				("supergroup" :: Text) -> Supergroup <$> c .: "id" <*> c .: "title" <*> v .: "forward_from"
				_ -> fail "Type of chat is not defined"

		reply :: Object -> Parser Message
		reply v = Replied <$> v .: "message_id" <*> parseJSON (Object v)
			<*> parseJSON (Object v) <*> v .: "reply_to_message"

		direct :: Object -> Parser Message
		direct v = Direct <$> v .: "message_id"
			<*> parseJSON (Object v) <*> parseJSON (Object v)

instance Identifiable Message where
	type instance Identificator Message = Int
	ident (Direct i _ _) = i
	ident (Forwarded i _ _) = i
	ident (Replied i _ _ _) = i

data Forward a = Forward Int Int64 Int64

instance Persistable (Forward Message) where
	type instance Payload (Forward Message) = Forward Message
	payload (Forward message_id from_chat_id to_chat_id) = field "message_id" message_id
		<> field "from_chat_id" from_chat_id <> field "chat_id" to_chat_id
	endpoint _ = "forwardMessage"

data Send a = Send Int64 a

instance Persistable (Send Text) where
	type instance Payload (Send Text) = Send Text
	payload (Send chat_id text) = field "chat_id" chat_id <> field "text" text
	endpoint _ = "sendMessage"

instance Persistable (Send (Text :&: Keyboard)) where
	type instance Payload (Send (Text :&: Keyboard)) = Send (Text :&: Keyboard)
	payload (Send chat_id (text :&: reply_markup)) = field "chat_id" chat_id
		<> field "text" text <> field "reply_markup" reply_markup
	endpoint _ = "sendMessage"

instance Persistable (Send Audio) where
	type instance Payload (Send Audio) = Send (URI :&: Audio)
	payload (Send chat_id (uri :&: Audio duration performer title mime_type file_size)) = field "file_id" uri
		<> field "chat_id" chat_id <> field "duration" duration <> field "performer" performer
		<> field "title" title <> field "mime_type" mime_type <> field "file_size" file_size
	endpoint _ = "sendAudio"

instance Persistable (Send (Caption :&: Audio)) where
	type instance Payload (Send (Caption :&: Audio)) = Send (Caption :&: URI :&: Audio)
	payload (Send chat_id (caption :&: audio)) = payload (Send chat_id audio) <> field "caption" caption
	endpoint _ = "sendAudio"

instance Persistable (Send Document) where
	type instance Payload (Send Document) = Send (URI :&: Document)
	payload (Send chat_id (uri :&: Document file_name mime_type file_size)) = field "file_id" uri
		<> field "chat_id" chat_id <> field "file_name" file_name
		<> field "mime_type" mime_type <> field "file_size" file_size
	endpoint _ = "sendDocument"

instance Persistable (Send (Caption :&: Document)) where
	type instance Payload (Send (Caption :&: Document)) = Send (Caption :&: URI :&: Document)
	payload (Send chat_id (caption :&: document)) = payload (Send chat_id document) <> field "caption" caption
	endpoint _ = "sendDocument"

instance Persistable (Send Video) where
	type instance Payload (Send Video) = Send (URI :&: Video)
	payload (Send chat_id (uri :&: Video width height duration mime_type file_size)) = field "file_id" uri
		<> field "chat_id" chat_id <> field "duration" duration <> field "width" width
		<> field "height" height <> field "mime_type" mime_type <> field "file_size" file_size
	endpoint _ = "sendVideo"

instance Persistable (Send (Caption :&: Video)) where
	type instance Payload (Send (Caption :&: Video)) = Send (Caption :&: URI :&: Video)
	payload (Send chat_id (caption :&: video)) = payload (Send chat_id video) <> field "caption" caption
	endpoint _ = "sendVideo"

instance Persistable (Send Voice) where
	type instance Payload (Send Voice) = Send (URI :&: Voice)
	payload (Send chat_id (uri :&: Voice duration mime_type file_size)) = field "file_id" uri
		<> field "chat_id" chat_id <> field "duration" duration
		<> field "mime_type" mime_type <> field "file_size" file_size
	endpoint _ = "sendVoice"

instance Persistable (Send (Caption :&: Voice)) where
	type instance Payload (Send (Caption :&: Voice)) = Send (Caption :&: URI :&: Voice)
	payload (Send chat_id (caption :&: voice)) = payload (Send chat_id voice) <> field "caption" caption
	endpoint _ = "sendVoice"

instance Persistable (Send Location) where
	type instance Payload (Send Location) = Send Location
	payload (Send chat_id (Location latitude longitude)) = field "chat_id" chat_id
		<> field "latitude" latitude <> field "longitude" longitude
	endpoint _ = "sendLocation"

instance Persistable (Send (Live Location)) where
	type instance Payload (Send (Live Location)) = Send (Live Location)
	payload (Send chat_id (Live live_period (Location latitude longitude))) =
		field "chat_id" chat_id <> field "live_period" live_period
		<> field "latitude" latitude <> field "longitude" longitude
	endpoint _ = "sendLocation"

instance Persistable (Send Poll) where
	type instance Payload (Send Poll) = Send Poll
	payload (Send chat_id (Opened question options)) = field "chat_id" chat_id
		<> field "question" question <> field "options" options
	payload (Send chat_id (Closed question options)) = field "chat_id" chat_id
		<> field "question" question <> field "options" options
	endpoint _ = "sendPoll"

data Reply a = Reply Int a

instance Persistable (Send a) => Persistable (Reply a) where
	type Payload (Reply a) = Reply (Payload (Send a))
	payload (Reply reply_to_message_id x) = payload x <> field
		"reply_to_message_id" reply_to_message_id
	endpoint (Reply _ x) = endpoint x

data Edit b = Edit Int64 Int b

instance Persistable (Edit Text) where
	type Payload (Edit Text) = Edit Text
	payload (Edit chat_id message_id text) = field "chat_id" chat_id
		<> field "message_id" message_id <> field "text" text
	endpoint _ = "editMessageText"

instance Persistable (Edit Keyboard) where
	type Payload (Edit Keyboard) = Edit Keyboard
	payload (Edit chat_id message_id reply_markup) = field "chat_id" chat_id
		<> field "message_id" message_id <> field "reply_markup" reply_markup
	endpoint _ = "editMessageText"

instance Persistable (Edit (Text :&: Keyboard)) where
	type Payload (Edit (Text :&: Keyboard)) = Edit (Text :&: Keyboard)
	payload (Edit chat_id message_id (text :&: reply_markup)) =
		field "chat_id" chat_id <> field "message_id" message_id
		<> field "text" text <> field "reply_markup" reply_markup
	endpoint _ = "editMessageText"

instance Persistable (Edit (Live Location)) where
	type instance Payload (Edit (Live Location)) = Edit Location
	payload (Edit chat_id message_id (Location latitude longitude)) =
		field "chat_id" chat_id <> field "message_id" message_id
		<> field "latitude" latitude <> field "longitude" longitude
	endpoint _ = "editMessageLiveLocation"

data Delete a = Delete Int64 Int

instance Persistable (Delete Message) where
	type Payload (Delete Message) = Delete Message
	payload (Delete chat_id message_id) = field "chat_id" chat_id
		<> field "message_id" message_id
	endpoint _ = "deleteMessage"

data Stop a = Stop Int64 Int

instance Persistable (Stop (Live Location)) where
	type instance Payload (Stop (Live Location)) = Stop (Live Location)
	payload (Stop chat_id message_id) = field "chat_id" chat_id
		<> field "message_id" message_id
	endpoint _ = "stopMessageLiveLocation"

instance Persistable (Stop Poll) where
	type instance Payload (Stop Poll) = Stop (Stop Poll)
	payload (Stop chat_id message_id) = field "chat_id" chat_id
		<> field "message_id" message_id
	endpoint _ = "stopPoll"

data Silently (todo :: * -> *) a = Silently a

instance Persistable (Forward obj) => Persistable (Silently Forward obj) where
	type Payload (Silently Forward obj) = Silently Forward (Payload (Forward obj))
	payload (Silently x) = payload x <> field "disable_notification" True
	endpoint (Silently x) = endpoint x

instance Persistable (Send obj) => Persistable (Silently Send obj) where
	type Payload (Silently Send obj) = Silently Send (Payload (Send obj))
	payload (Silently x) = payload x <> field "disable_notification" True
	endpoint (Silently x) = endpoint x

instance Persistable (Reply obj) => Persistable (Silently Reply obj) where
	type Payload (Silently Reply obj) = Silently Reply (Payload (Reply obj))
	payload (Silently x) = payload x <> field "disable_notification" True
	endpoint (Silently x) = endpoint x
