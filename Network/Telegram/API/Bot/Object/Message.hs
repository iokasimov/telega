module Network.Telegram.API.Bot.Object.Message (Message (..), module Exports) where

import Network.Telegram.API.Bot.Object.Message.Content as Exports
import Network.Telegram.API.Bot.Object.Message.From as Exports
import Network.Telegram.API.Bot.Object.Message.Origin as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object), withObject, (.:))
import "aeson" Data.Aeson.Types (Object, Parser)
import "base" Control.Applicative (Applicative ((<*>)), Alternative ((<|>)))
import "base" Control.Monad (Monad ((>>=)), fail)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.Telegram.API.Bot.Object.Message.Content (Content)
import Network.Telegram.API.Bot.Object.Message.Origin (Origin (Private, Group, Supergroup, Channel))

data Message
	= Direct Int Origin Content
	| Forward Int Origin Content
	deriving Show

instance FromJSON Message where
	parseJSON = withObject "Message" $ \v ->
		forward_from_channel v <|> forward_from_chat v <|> direct v where

		forward_from_channel :: Object -> Parser Message
		forward_from_channel v = Forward <$> v .: "forward_from_message_id"
			<*> (v .: "forward_from_chat" >>= channel) <*> parseJSON (Object v) where

			channel :: Value -> Parser Origin
			channel = withObject "Channel" $ \c -> Channel <$> c .: "id" <*> c .: "title"

		forward_from_chat :: Object -> Parser Message
		forward_from_chat v = Forward <$> v .: "message_id"
			<*> (v .: "chat" >>= chat) <*> parseJSON (Object v) where

			chat :: Value -> Parser Origin
			chat = withObject "Origin" $ \c -> c .: "type" >>= \case
				("private" :: Text) -> Private <$> c .: "id" <*> v .: "forward_from"
				("group" :: Text) -> Group <$> c .: "id" <*> c .: "title" <*> v .: "forward_from"
				("supergroup" :: Text) -> Supergroup <$> c .: "id" <*> c .: "title" <*> v .: "forward_from"
				_ -> fail "Type of chat is not defined"

		direct :: Object -> Parser Message
		direct v = Direct <$> v .: "message_id"
			<*> parseJSON (Object v) <*> parseJSON (Object v)
