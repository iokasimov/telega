module Network.API.Telegram.Bot.Object.Update.Message.Keyboard (Keyboard (..), module Exports) where

import Network.API.Telegram.Bot.Object.Update.Message.Keyboard.Button as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int, Int64)
import "base" Text.Show (Show)
import "tagged" Data.Tagged (Tagged, untag)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Property.Persistable
	(Persistable (Payload, payload, endpoint), Capacity (Edit, Post))

data Keyboard = Inline [[Button]] deriving Show

instance FromJSON Keyboard where
	parseJSON = withObject "Inline" $ \v ->
		Inline <$> v .: "inline_keyboard"

instance ToJSON Keyboard where
	toJSON (Inline buttons) = object
		["inline_keyboard" .= buttons]

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
