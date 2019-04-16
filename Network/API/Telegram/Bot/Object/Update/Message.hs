module Network.API.Telegram.Bot.Object.Update.Message (Message (..), module Exports) where

import Network.API.Telegram.Bot.Object.Update.Message.Content as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Keyboard as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Origin as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object), object, withObject, (.:), (.=))
import "aeson" Data.Aeson.Types (Object, Parser)
import "base" Control.Applicative (Applicative ((<*>)), Alternative ((<|>)))
import "base" Control.Monad (Monad ((>>=)), fail)
import "base" Data.Bool (Bool (True, False))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int, Int64)
import "base" Text.Show (Show)
import "tagged" Data.Tagged (Tagged, untag)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Object.Update.Message.Content (Content)
import Network.API.Telegram.Bot.Object.Update.Message.Origin (Origin (Private, Group, Supergroup, Channel))
import Network.API.Telegram.Bot.Property.Persistable (Persistable (Payload, payload, endpoint)
	, Capacity (Send, Edit, Purge), Inform (Notify, Silently), Way (Directly, Forwarding, Replying))

data Message
	= Direct Int Origin Content
	| Forward Int Origin Content
	| Reply Int Origin Content Message
	deriving Show

instance FromJSON Message where
	parseJSON = withObject "Message" $ \v ->
		forward_channel v <|> forward_chat v <|> reply v <|> direct v where

		forward_channel :: Object -> Parser Message
		forward_channel v = Forward <$> v .: "forward_from_message_id"
			<*> (v .: "forward_from_chat" >>= channel) <*> parseJSON (Object v) where

			channel :: Value -> Parser Origin
			channel = withObject "Channel" $ \c -> Channel <$> c .: "id" <*> c .: "title"

		forward_chat :: Object -> Parser Message
		forward_chat v = Forward <$> v .: "message_id"
			<*> (v .: "chat" >>= chat) <*> parseJSON (Object v) where

			chat :: Value -> Parser Origin
			chat = withObject "Origin" $ \c -> c .: "type" >>= \case
				("private" :: Text) -> Private <$> c .: "id" <*> v .: "forward_from"
				("group" :: Text) -> Group <$> c .: "id" <*> c .: "title" <*> v .: "forward_from"
				("supergroup" :: Text) -> Supergroup <$> c .: "id" <*> c .: "title" <*> v .: "forward_from"
				_ -> fail "Type of chat is not defined"

		reply :: Object -> Parser Message
		reply v = Reply <$> v .: "message_id" <*> parseJSON (Object v)
			<*> parseJSON (Object v) <*> v .: "reply_to_message"

		direct :: Object -> Parser Message
		direct v = Direct <$> v .: "message_id"
			<*> parseJSON (Object v) <*> parseJSON (Object v)

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
