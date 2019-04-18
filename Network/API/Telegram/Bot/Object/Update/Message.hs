module Network.API.Telegram.Bot.Object.Update.Message (Message (..), module Exports) where

import Network.API.Telegram.Bot.Object.Update.Message.Content as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Keyboard as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Origin as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Object), withObject, (.:))
import "aeson" Data.Aeson.Types (Object, Parser)
import "base" Control.Applicative (Applicative ((<*>)), Alternative ((<|>)))
import "base" Control.Monad (Monad ((>>=)), fail)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int, Int64)
import "base" Data.Semigroup ((<>))
import "base" Text.Show (Show)
import "text" Data.Text (Text)
import "unordered-containers" Data.HashMap.Strict (singleton)
import "with" Data.With (type (:&:)((:&:)))

import Network.API.Telegram.Bot.Object.Update.Message.Content (Content)
import Network.API.Telegram.Bot.Object.Update.Message.Origin (Origin (Private, Group, Supergroup, Channel))
import Network.API.Telegram.Bot.Property.Accessible (Accessible (access))
import Network.API.Telegram.Bot.Property.Identifiable (Identifiable (Identificator, ident))
import Network.API.Telegram.Bot.Property.Persistable (Persistable (Payload, payload, endpoint), Capacity (Send))

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

instance Persistable ('Send Message) where
	type instance Payload ('Send Message) = (Int64 :&: Text)
	payload (chat_id :&: text) = singleton "chat_id" (toJSON chat_id) <> singleton "text" (toJSON text)
	endpoint _ = "sendMessage"
