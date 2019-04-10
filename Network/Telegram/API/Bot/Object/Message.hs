module Network.Telegram.API.Bot.Object.Message (Message (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object), withObject, (.:))
import "aeson" Data.Aeson.Types (Object, Parser)
import "base" Control.Applicative (Applicative ((<*>)), Alternative ((<|>)))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)
import "base" Text.Show (Show)

import Network.Telegram.API.Bot.Object.Chat (Chat)
import Network.Telegram.API.Bot.Object.Content (Content)
import Network.Telegram.API.Bot.Object.From (From)

data Message
	= Direct Int Chat From Content
	| Forward Int Chat From Content
	deriving Show

instance FromJSON Message where
	parseJSON = withObject "Message" $ \v ->
		forward_from_channel v <|> forward_from v <|> direct v where

		direct :: Object -> Parser Message
		direct v = Direct <$> v .: "message_id" <*> v .: "chat"
			<*> v .: "from" <*> parseJSON (Object v)

		forward_from_channel :: Object -> Parser Message
		forward_from_channel v = Forward <$> v .: "forward_from_message_id"
			<*> v .: "forward_from_chat" <*> v .: "forward_from" <*> parseJSON (Object v)

		forward_from :: Object -> Parser Message
		forward_from v = Forward <$> v .: "message_id" <*> v .: "chat"
			<*> v .: "forward_from" <*> parseJSON (Object v)
