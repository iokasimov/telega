module Network.API.Telegram.Bot.Object.Update.Message (module Exports
	, Message (..), ID (MSG), Send (..), Reply (..), Forward (..)
	, Edit (..), Delete (..), Pin (..), Unpin (..), Silently (..)) where

import Network.API.Telegram.Bot.Object.Update.Message.Content as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Keyboard as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Origin as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Object), withObject, (.:))
import "aeson" Data.Aeson.Types (Object, Parser)
import "base" Control.Applicative ((<*>), (<|>))
import "base" Control.Monad (fail, (>>=))
import "base" Data.Bool (Bool (True))
import "base" Data.Eq (Eq ((==)))
import "base" Data.Function (flip, ($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)
import "base" Data.Semigroup ((<>))
import "text" Data.Text (Text)
import "with" Data.With (type (:&:)((:&:)))

import Network.API.Telegram.Bot.Field (Caption, URI)
import Network.API.Telegram.Bot.Object.Chat (Chat, Group, Channel, ID)
import Network.API.Telegram.Bot.Property.Accessible (Accessible (access))
import Network.API.Telegram.Bot.Property.Identifiable (Identifiable (Identificator, ident))
import Network.API.Telegram.Bot.Property.Persistable (Persistable (Payload, Returning, payload, endpoint))
import Network.API.Telegram.Bot.Utils (field)

data Message
	= Direct (ID Message) Origin Content
	| Forwarded (ID Message) Origin Content
	| Replied (ID Message) Origin Content Message

instance Eq Message where
	m == m' = ident m == ident m'

instance Accessible Content Message where
	access f (Direct msg_id origin content) = (\content' -> Direct msg_id origin content') <$> f content
	access f (Forwarded msg_id origin content) = (\content' -> Forwarded msg_id origin content') <$> f content
	access f (Replied msg_id origin content msg) = (\content' -> Replied msg_id origin content' msg) <$> f content

instance Accessible Origin Message where
	access f (Direct msg_id origin content) = (\origin' -> Direct msg_id origin' content) <$> f origin
	access f (Forwarded msg_id origin content) = (\origin' -> Forwarded msg_id origin' content) <$> f origin
	access f (Replied msg_id origin content msg) = (\origin' -> Replied msg_id origin' content msg) <$> f origin

instance Accessible (ID Chat) Message where
	access f (Direct msg_id origin content) = flip (Direct msg_id) content <$> access f origin
	access f (Forwarded msg_id origin content) = flip (Forwarded msg_id) content <$> access f origin
	access f (Replied msg_id origin content msg) = Replied msg_id origin content <$> access f msg

instance Identifiable Message where
	type Identificator Message = ID Message
	ident (Direct i _ _) = i
	ident (Forwarded i _ _) = i
	ident (Replied i _ _ _) = i

instance FromJSON Message where
	parseJSON = withObject "Message" $ \v ->
		forward_channel v <|> forward_chat v <|> reply v <|> direct v where

		forward_channel :: Object -> Parser Message
		forward_channel v = Forwarded <$> v .: "forward_from_message_id"
			<*> (v .: "forward_from_chat" >>= channel) <*> parseJSON (Object v) where

			channel :: Value -> Parser Origin
			channel = withObject "Channel" $ \c -> Blog
				<$> c .: "id" <*> parseJSON (Object c)

		forward_chat :: Object -> Parser Message
		forward_chat v = Forwarded <$> v .: "message_id"
			<*> (v .: "chat" >>= chat) <*> parseJSON (Object v) where

			chat :: Value -> Parser Origin
			chat = withObject "Origin" $ \c -> c .: "type" >>= \case
				("private" :: Text) -> Private <$> c .: "id" <*> v .: "forward_from"
				("group" :: Text) -> Group <$> c .: "id" <*> parseJSON (Object c) <*> v .: "forward_from"
				("supergroup" :: Text) -> Group <$> c .: "id" <*> parseJSON (Object c) <*> v .: "forward_from"
				_ -> fail "Type of chat is not defined"

		reply :: Object -> Parser Message
		reply v = Replied <$> v .: "message_id" <*> parseJSON (Object v)
			<*> parseJSON (Object v) <*> v .: "reply_to_message"

		direct :: Object -> Parser Message
		direct v = Direct <$> v .: "message_id"
			<*> parseJSON (Object v) <*> parseJSON (Object v)

data Forward a = Forward (ID Message) (ID Chat) (ID Chat)

instance Persistable (Forward Message) where
	type Payload (Forward Message) = Forward Message
	type Returning (Forward Message) = Message
	payload (Forward message_id from_chat_id to_chat_id) = field "message_id" message_id
		<> field "from_chat_id" from_chat_id <> field "chat_id" to_chat_id
	endpoint _ = "forwardMessage"

data Send a = Send (ID Chat) a

instance Persistable (Send Text) where
	type Payload (Send Text) = Send Text
	type Returning (Send Text) = Message
	payload (Send chat_id text) = field "chat_id" chat_id <> field "text" text
	endpoint _ = "sendMessage"

instance Persistable (Send (Text :&: Keyboard)) where
	type Payload (Send (Text :&: Keyboard)) = Send (Text :&: Keyboard)
	type Returning (Send (Text :&: Keyboard)) = Message
	payload (Send chat_id (text :&: reply_markup)) = field "chat_id" chat_id
		<> field "text" text <> field "reply_markup" reply_markup
	endpoint _ = "sendMessage"

instance Persistable (Send Audio) where
	type Payload (Send Audio) = Send Audio
	type Returning (Send Audio) = Message
	payload (Send chat_id (Audio uri duration performer title mime_type file_size)) = field "file_id" uri
		<> field "chat_id" chat_id <> field "duration" duration <> field "performer" performer
		<> field "title" title <> field "mime_type" mime_type <> field "file_size" file_size
	endpoint _ = "sendAudio"

instance Persistable (Send (Caption :&: Audio)) where
	type Payload (Send (Caption :&: Audio)) = Send (Caption :&: Audio)
	type Returning (Send (Caption :&: Audio)) = Message
	payload (Send chat_id (caption :&: audio)) = payload (Send chat_id audio) <> field "caption" caption
	endpoint _ = "sendAudio"

instance Persistable (Send Document) where
	type Payload (Send Document) = Send Document
	type Returning (Send Document) = Message
	payload (Send chat_id (Document uri file_name mime_type file_size)) = field "file_id" uri
		<> field "chat_id" chat_id <> field "file_name" file_name
		<> field "mime_type" mime_type <> field "file_size" file_size
	endpoint _ = "sendDocument"

instance Persistable (Send (Caption :&: Document)) where
	type Payload (Send (Caption :&: Document)) = Send (Caption :&: Document)
	type Returning (Send (Caption :&: Document)) = Message
	payload (Send chat_id (caption :&: document)) = payload (Send chat_id document) <> field "caption" caption
	endpoint _ = "sendDocument"

instance Persistable (Send Video) where
	type Payload (Send Video) = Send Video
	type Returning (Send Video) = Message
	payload (Send chat_id (Video uri width height duration mime_type file_size)) = field "file_id" uri
		<> field "chat_id" chat_id <> field "duration" duration <> field "width" width
		<> field "height" height <> field "mime_type" mime_type <> field "file_size" file_size
	endpoint _ = "sendVideo"

instance Persistable (Send (Caption :&: Video)) where
	type Payload (Send (Caption :&: Video)) = Send (Caption :&: Video)
	type Returning (Send (Caption :&: Video)) = Message
	payload (Send chat_id (caption :&: video)) = payload (Send chat_id video) <> field "caption" caption
	endpoint _ = "sendVideo"

instance Persistable (Send Voice) where
	type Payload (Send Voice) = Send Voice
	type Returning (Send Voice) = Message
	payload (Send chat_id (Voice uri duration mime_type file_size)) = field "file_id" uri
		<> field "chat_id" chat_id <> field "duration" duration
		<> field "mime_type" mime_type <> field "file_size" file_size
	endpoint _ = "sendVoice"

instance Persistable (Send (Caption :&: Voice)) where
	type Payload (Send (Caption :&: Voice)) = Send (Caption :&: Voice)
	type Returning (Send (Caption :&: Voice)) = Message
	payload (Send chat_id (caption :&: voice)) = payload (Send chat_id voice) <> field "caption" caption
	endpoint _ = "sendVoice"

instance Persistable (Send Photo) where
	type Payload (Send Photo) = Send URI
	type Returning (Send Photo) = Message
	payload (Send chat_id uri) = field "chat_id" chat_id <> field "photo" uri
	endpoint _ = "sendPhoto"

instance Persistable (Send (Caption :&: Photo)) where
	type Payload (Send (Caption :&: Photo)) = Send (Caption :&: URI)
	type Returning (Send (Caption :&: Photo)) = Message
	payload (Send chat_id (caption :&: uri)) = field "chat_id" chat_id
		<> field "photo" uri <> field "caption" caption
	endpoint _ = "sendPhoto"

instance Persistable (Send Location) where
	type Payload (Send Location) = Send Location
	type Returning (Send Location) = Message
	payload (Send chat_id (Location latitude longitude)) = field "chat_id" chat_id
		<> field "latitude" latitude <> field "longitude" longitude
	endpoint _ = "sendLocation"

instance Persistable (Send (Live Location)) where
	type Payload (Send (Live Location)) = Send (Live Location)
	type Returning (Send (Live Location)) = Message
	payload (Send chat_id (Live live_period (Location latitude longitude))) =
		field "chat_id" chat_id <> field "live_period" live_period
		<> field "latitude" latitude <> field "longitude" longitude
	endpoint _ = "sendLocation"

instance Persistable (Send Poll) where
	type Payload (Send Poll) = Send Poll
	type Returning (Send Poll) = Message
	payload (Send chat_id (Poll question options)) = field "chat_id" chat_id
		<> field "question" question <> field "options" options
	endpoint _ = "sendPoll"

data Reply a = Reply (ID Message) a

instance Persistable (Send a) => Persistable (Reply a) where
	type Payload (Reply a) = Reply (Payload (Send a))
	type Returning (Reply a) = Returning (Send a)
	payload (Reply reply_to_message_id x) = payload x <> field
		"reply_to_message_id" reply_to_message_id
	endpoint (Reply _ x) = endpoint x

data Edit b = Edit (ID Chat) (ID Message) b

instance Persistable (Edit Text) where
	type Payload (Edit Text) = Edit Text
	type Returning (Edit Text) = Message
	payload (Edit chat_id message_id text) = field "chat_id" chat_id
		<> field "message_id" message_id <> field "text" text
	endpoint _ = "editMessageText"

instance Persistable (Edit Keyboard) where
	type Payload (Edit Keyboard) = Edit Keyboard
	type Returning (Edit Keyboard) = Message
	payload (Edit chat_id message_id reply_markup) = field "chat_id" chat_id
		<> field "message_id" message_id <> field "reply_markup" reply_markup
	endpoint _ = "editMessageReplyMarkup"

instance Persistable (Edit (Live Location)) where
	type Payload (Edit (Live Location)) = Edit Location
	type Returning (Edit (Live Location)) = Message
	payload (Edit chat_id message_id (Location latitude longitude)) =
		field "chat_id" chat_id <> field "message_id" message_id
		<> field "latitude" latitude <> field "longitude" longitude
	endpoint _ = "editMessageLiveLocation"

data Delete a = Delete (ID Chat) (ID Message)

instance Persistable (Delete Message) where
	type Payload (Delete Message) = Delete Message
	type Returning (Delete Message) = ()
	payload (Delete chat_id message_id) = field "chat_id" chat_id
		<> field "message_id" message_id
	endpoint _ = "deleteMessage"

data Stop a = Stop (ID Chat) (ID Message)

instance Persistable (Stop (Live Location)) where
	type Payload (Stop (Live Location)) = Stop (Live Location)
	type Returning (Stop (Live Location)) = Message
	payload (Stop chat_id message_id) = field "chat_id" chat_id
		<> field "message_id" message_id
	endpoint _ = "stopMessageLiveLocation"

instance Persistable (Stop Poll) where
	type Payload (Stop Poll) = Stop Poll
	type Returning (Stop Poll) = Poll
	payload (Stop chat_id message_id) = field "chat_id" chat_id
		<> field "message_id" message_id
	endpoint _ = "stopPoll"

data Pin chat msg where
	Pin :: ID Message -> ID Chat -> Pin b Message

instance Persistable (Pin Group Message) where
	type Payload (Pin Group Message) = Pin Group Message
	type Returning (Pin Group Message) = ()
	payload (Pin chat_id message_id) = field "chat_id" chat_id
		<> field "message_id" message_id
	endpoint _ = "pinChatMessage"

instance Persistable (Pin Channel Message) where
	type Payload (Pin Channel Message) = Pin Channel Message
	type Returning (Pin Channel Message) = ()
	payload (Pin chat_id message_id) = field "chat_id" chat_id
		<> field "message_id" message_id
	endpoint _ = "pinChatMessage"

data Unpin chat msg where
	Unpin :: ID Chat -> Unpin b Message

instance Persistable (Unpin Group Message) where
	type Payload (Unpin Group Message) = Unpin Group Message
	type Returning (Unpin Group Message) = ()
	payload (Unpin chat_id) = field "chat_id" chat_id
	endpoint _ = "unpinChatMessage"

instance Persistable (Unpin Channel Message) where
	type Payload (Unpin Channel Message) = Unpin Channel Message
	type Returning (Unpin Channel Message) = ()
	payload (Unpin chat_id) = field "chat_id" chat_id
	endpoint _ = "unpinChatMessage"

data Silently (todo :: * -> *) a = Silently a

instance Persistable (Forward obj) => Persistable (Silently Forward obj) where
	type Payload (Silently Forward obj) = Silently Forward (Payload (Forward obj))
	type Returning (Silently Forward obj) = Message
	payload (Silently x) = payload x <> field "disable_notification" True
	endpoint (Silently x) = endpoint x

instance Persistable (Send obj) => Persistable (Silently Send obj) where
	type Payload (Silently Send obj) = Silently Send (Payload (Send obj))
	type Returning (Silently Send obj) = Message
	payload (Silently x) = payload x <> field "disable_notification" True
	endpoint (Silently x) = endpoint x

instance Persistable (Reply obj) => Persistable (Silently Reply obj) where
	type Payload (Silently Reply obj) = Silently Reply (Payload (Reply obj))
	type Returning (Silently Reply obj) = Message
	payload (Silently x) = payload x <> field "disable_notification" True
	endpoint (Silently x) = endpoint x

instance Persistable (Pin chat Message) => Persistable (Silently (Pin chat) Message) where
	type Payload (Silently (Pin chat) Message) = Silently (Pin chat) (Payload (Pin chat Message))
	type Returning (Silently (Pin chat) Message) = ()
	payload (Silently x) = payload x <> field "disable_notification" True
	endpoint (Silently x) = endpoint x

instance Persistable (Unpin chat Message) => Persistable (Silently (Unpin chat) Message) where
	type Payload (Silently (Unpin chat) Message) = Silently (Unpin chat) (Payload (Unpin chat Message))
	type Returning (Silently (Unpin chat) Message) = ()
	payload (Silently x) = payload x <> field "disable_notification" True
	endpoint (Silently x) = endpoint x

data instance ID Message = MSG Int

deriving instance Eq (ID Message)

instance FromJSON (ID Message) where parseJSON o = MSG <$> parseJSON o
instance ToJSON (ID Message) where toJSON (MSG i) = toJSON i
