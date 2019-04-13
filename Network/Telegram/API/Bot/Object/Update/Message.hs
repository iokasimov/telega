module Network.Telegram.API.Bot.Object.Update.Message (Message (..), Messaging (..), module Exports) where

import Network.Telegram.API.Bot.Object.Update.Message.Content as Exports
import Network.Telegram.API.Bot.Object.Update.Message.Keyboard as Exports
import Network.Telegram.API.Bot.Object.Update.Message.Origin as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object), withObject, (.:))
import "aeson" Data.Aeson.Types (Object, Parser)
import "base" Control.Applicative (Applicative ((<*>)), Alternative ((<|>)))
import "base" Control.Monad (Monad ((>>=)), fail)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.Telegram.API.Bot.Object.Update.Message.Content (Content)
import Network.Telegram.API.Bot.Object.Update.Message.Origin (Origin (Private, Group, Supergroup, Channel))

data Messaging = Directly | Forwarding | Replying

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
